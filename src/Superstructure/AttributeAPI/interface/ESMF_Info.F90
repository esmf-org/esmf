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
#define ESMF_FILENAME "ESMF_Info.F90"
!==============================================================================
!
! ESMF Info module
module ESMF_InfoMod
!
!==============================================================================
!
! This file contains the Info class definition and methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_InfoMod - Info class.
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt ESMF\_Info} class and associated functions and subroutines.  
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
! ! ESMF_Info    

  type ESMF_Info
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    !private       
    type (ESMF_Base)              :: base             ! base class object
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Info

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public operator(==)
  public operator(/=)

  public ESMF_InfoCreate
  public ESMF_InfoDestroy
  public ESMF_InfoIsCreated
  
! - ESMF-internal methods:
  public ESMF_InfoGetInit

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
! InfoOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_InfoAssignment(=) - Info assignment
!
! !INTERFACE:
!   interface assignment(=)
!   info1 = info2
!
! !ARGUMENTS:
!   type(ESMF_Info) :: info1
!   type(ESMF_Info) :: info2
!
! !DESCRIPTION:
!   Assign info1 as an alias to the same ESMF Info object in memory
!   as info2. If info2 is invalid, then info1 will be equally 
!   invalid after the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[ingo1]
!     The {\tt ESMF\_Info} object on the left hand side of the assignment.
!   \item[info2]
!     The {\tt ESMF\_Info} object on the right hand side of the assignment.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_InfoOperator(==) - Info equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (info1 == info2) then ... endif
!             OR
!   result = (info1 == info2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_Info), intent(in) :: info1
!   type(ESMF_Info), intent(in) :: info2
!
! !DESCRIPTION:
!   Test whether info1 and info2 are valid aliases to the same ESMF
!   Info object in memory. For a more general comparison of two ESMF 
!   Infos, going beyond the simple alias test, the ESMF\_InfoMatch() 
!   function (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[info1]
!     The {\tt ESMF\_Info} object on the left hand side of the equality
!     operation.
!   \item[info2]
!     The {\tt ESMF\_Info} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOPI
    module procedure ESMF_InfoEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_InfoOperator(/=) - Info not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (info1 /= info2) then ... endif
!             OR
!   result = (info1 /= info2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_Info), intent(in) :: info1
!   type(ESMF_Info), intent(in) :: info2
!
! !DESCRIPTION:
!   Test whether info1 and info2 are {\it not} valid aliases to the
!   same ESMF Info object in memory. For a more general comparison of two 
!   ESMF Infos, going beyond the simple alias test, the ESMF\_InfoMatch()
!   function (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[info1]
!     The {\tt ESMF\_Info} object on the left hand side of the non-equality
!     operation.
!   \item[info2]
!     The {\tt ESMF\_Info} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOPI
    module procedure ESMF_InfoNE

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
#define ESMF_METHOD "ESMF_InfoEQ()"
!BOPI
! !IROUTINE:  ESMF_InfoEQ - Compare two Infos for equality
!
! !INTERFACE:
  function ESMF_InfoEQ(info1, info2)
! 
! !RETURN VALUE:
    logical :: ESMF_InfoEQ

! !ARGUMENTS:
    type(ESMF_Info), intent(in) :: info1
    type(ESMF_Info), intent(in) :: info2

!
! !DESCRIPTION:
!   Test if both {\tt info1} and {\tt info2} alias the same ESMF Info 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE infoinit1, infoinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    infoinit1 = ESMF_InfoGetInit(info1)
    infoinit2 = ESMF_InfoGetInit(info2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (infoinit1 .eq. ESMF_INIT_CREATED .and. &
      infoinit2 .eq. ESMF_INIT_CREATED) then
      ESMF_InfoEQ = (info1%base%this == info2%base%this)
    else
      ESMF_InfoEQ = ESMF_FALSE
    endif

  end function ESMF_InfoEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoNE()"
!BOPI
! !IROUTINE:  ESMF_InfoNE - Compare two Infos for non-equality
!
! !INTERFACE:
  function ESMF_InfoNE(info1, info2)
! 
! !RETURN VALUE:
    logical :: ESMF_InfoNE

! !ARGUMENTS:
    type(ESMF_Info), intent(in) :: info1
    type(ESMF_Info), intent(in) :: info2

!
! !DESCRIPTION:
!   Test if both {\tt info1} and {\tt info2} alias the same ESMF Info 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE infoinit1, infoinit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    
    ESMF_InfoNE = .not.ESMF_InfoEQ(info1, info2)

  end function ESMF_InfoNE
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoCreate"
!BOPI
! !IROUTINE: ESMF_InfoCreate - Create an Info object
!
! !INTERFACE:
  recursive function ESMF_InfoCreate(keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_Info) :: ESMF_InfoCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                 intent(out),   optional :: rc
!
! !DESCRIPTION:
! This interface creates an {\tt ESMF\_Info} object. 
! The return value is the new {\tt ESMF\_Info}.
!   
! The arguments are:
! \begin{description}
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                       :: localrc         ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ! create base member
    call ESMF_BaseCreate(ESMF_InfoCreate%base, "Info", rc=localrc)
    if (ESMF_LogFoundAllocError(localrc, msg="compclass", &
      ESMF_CONTEXT, rcTOReturn=rc)) return
      
    ESMF_INIT_SET_CREATED(ESMF_InfoCreate)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_InfoCreate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoDestroy"
!BOPI
! !IROUTINE: ESMF_InfoDestroy - Release resources associated with an Info object
!
! !INTERFACE:
  subroutine ESMF_InfoDestroy(info, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_Info), intent(inout)          :: info
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,         intent(in),   optional :: noGarbage
    integer,         intent(out),  optional :: rc
!
! !DESCRIPTION:
! Destroys an {\tt ESMF\_Info}, releasing the resources associated
! with the object.
!
! The arguments are:
! \begin{description}
! \item[info]
!   Release all resources associated with this {\tt ESMF\_Info}
!   and mark the object as invalid.  It is an error to pass this
!   object into any other routines after being destroyed.
! \item[{[noGarbage]}]
!      If set to {\tt .TRUE.} the object will be fully destroyed and removed
!      from the ESMF garbage collection system. Note however that under this
!      condition ESMF cannot protect against accessing the destroyed object
!      through dangling aliases -- a situation which may lead to hard to debug
!      application crashes.
!
!      It is generally recommended to leave the {\tt noGarbage} argument
!      set to {\tt .FALSE.} (the default), and to take advantage of the ESMF
!      garbage collection system which will prevent problems with dangling
!      aliases or incorrect sequences of destroy calls. However this level of
!      support requires that a small remnant of the object is kept in memory
!      past the destroy call. This can lead to an unexpected increase in memory
!      consumption over the course of execution in applications that use
!      temporary ESMF objects. For situations where the repeated creation and
!      destruction of temporary objects leads to memory issues, it is
!      recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully
!      removing the entire temporary object from memory.
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                       ! local error status

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_InfoGetInit,info,rc)

    ! destroy base member
    call ESMF_BaseDestroy(info%base, noGarbage=noGarbage, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcTOReturn=rc)) return

    ! mark base member invalid
    call ESMF_BaseSetStatus(info%base, ESMF_STATUS_INVALID, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcTOReturn=rc)) return

    ESMF_INIT_SET_DELETED(info)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_InfoDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoIsCreated()"
!BOPI
! !IROUTINE: ESMF_InfoIsCreated - Check whether an Info object has been created

! !INTERFACE:
  function ESMF_InfoIsCreated(info, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_InfoIsCreated
!
! !ARGUMENTS:
    type(ESMF_Info), intent(in)            :: info
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,         intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt info} has been created. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[info]
!     {\tt ESMF\_Info} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
  !-----------------------------------------------------------------------------
    ESMF_InfoIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_InfoGetInit(info)==ESMF_INIT_CREATED) &
      ESMF_InfoIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InfoGetInit"
!BOPI
! !IROUTINE:  ESMF_InfoGetInit - Get initialization status.

! !INTERFACE:
  function ESMF_InfoGetInit(d)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_InfoGetInit
!
! !ARGUMENTS:
    type(ESMF_Info), intent(in), optional :: d
!
! !DESCRIPTION:
! Get the initialization status of the Deep class {\tt Info}.
!
! The arguments are:
! \begin{description}
! \item[d]
!   {\tt ESMF\_Info} from which to retrieve status.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(d)) then
      ESMF_InfoGetInit = ESMF_INIT_GET(d)
    else
      ESMF_InfoGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_InfoGetInit
!------------------------------------------------------------------------------

end module ESMF_InfoMod
