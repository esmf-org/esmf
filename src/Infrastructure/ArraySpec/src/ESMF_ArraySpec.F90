! $Id: ESMF_ArraySpec.F90,v 1.29.2.4 2009/01/21 21:25:19 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_ArraySpec.F90"
!==============================================================================
!
! ESMF ArraySpec Module
module ESMF_ArraySpecMod
!
!==============================================================================
!
! This file contains the ArraySpec class definition and all ArraySpec
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_ArraySpecMod - Manage data arrays uniformly between F90 and C++
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_ArraySpec} class and
! associated functions and subroutines.
!
! C and C++ arrays are simple pointers to memory.
! Fortran arrays contain shape and stride definitions and are strongly
! typed. To enable interoperability between the languages the C++ code
! must be able to obtain this information from the Fortran description
! (which is called the "dope vector" in Fortran), either through a priori
! knowledge or through query.
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
  ! Status of Array Spec
  type ESMF_ArraySpecStatus
    sequence
    integer  :: status
  end type

  ! Supported ESMF Array Spec Statuses:
  !    ESMF_ARRAYSPEC_STATUS_UNKNOWN   Not Known
  !    ESMF_ARRAYSPEC_STATUS_NOTSET    Array Spec hasn't been set yet
  !    ESMF_ARRAYSPEC_STATUS_SET       Array Spec has been set and can be used

   type (ESMF_ArraySpecStatus), parameter, private ::            &
      ESMF_ARRAYSPEC_STATUS_UNKNOWN  =  ESMF_ArraySpecStatus(0), &
      ESMF_ARRAYSPEC_STATUS_NOTSET   =  ESMF_ArraySpecStatus(1), &
      ESMF_ARRAYSPEC_STATUS_SET      =  ESMF_ArraySpecStatus(2)


!------------------------------------------------------------------------------
! ! ESMF_ArraySpec
!
! ! Data array specification, with no associated data buffer.
  type ESMF_ArraySpec
    sequence
    private
    integer             :: rank       ! number of dimensions
    type(ESMF_TypeKind) :: typekind   ! fortran type and kind enum/integer
#ifdef ESMF_NO_INITIALIZERS
    type (ESMF_ArraySpecStatus) :: status
#else
    type (ESMF_ArraySpecStatus) :: status = ESMF_ARRAYSPEC_STATUS_NOTSET 
#endif
    ESMF_INIT_DECLARE
  end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_ArraySpec

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArraySpecSet
  public ESMF_ArraySpecGet
  public ESMF_ArraySpecValidate

! - ESMF-internal methods:
  public ESMF_ArraySpecInit
  public ESMF_ArraySpecGetInit
  public ESMF_ArraySpecPrint
  private ESMF_ArraySpecStatusPrint

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_ArraySpec.F90,v 1.29.2.4 2009/01/21 21:25:19 cdeluca Exp $'

!==============================================================================

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_ArraySpecStatusEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF ArraySpec ids (enums).  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_ArraySpecStatusNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF ArraySpec ids (enums).  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!==============================================================================

  contains

!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecGet"
!BOP
! !IROUTINE: ESMF_ArraySpecGet - Get values from an ArraySpec
!
! !INTERFACE:
  subroutine ESMF_ArraySpecGet(arrayspec, rank, typekind, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout)         :: arrayspec
    integer,              intent(out), optional :: rank
    type(ESMF_TypeKind),  intent(out), optional :: typekind
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
! Returns information about the contents of an {\tt ESMF\_ArraySpec}.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
!   The {\tt ESMF\_ArraySpec} to query.
! \item[rank]
!   Array rank (dimensionality -- 1D, 2D, etc). Maximum possible is 7D.
! \item[typekind]
!   Array typekind.  See section \ref{opt:typekind} for valid values.
! \item[[rc]]
!  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    ! Local vars
    integer :: status ! local error status
    logical :: rcpresent ! did user specify rc?

    ! Initialize return code; assume routine is not implemented       
    status = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)

    ! check status
    if (arrayspec%status .ne. ESMF_ARRAYSPEC_STATUS_SET) then
       ! Use LogErr to handle return code (to record other info for logfile)
       if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_UNINIT, "ArraySpec hasn't been set", &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Get arrayspec contents
    if(present(rank)) rank = arrayspec%rank
    if(present(typekind)) typekind = arrayspec%typekind

    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecSet"
!BOP
! !IROUTINE: ESMF_ArraySpecSet - Set values for an ArraySpec
!
! !INTERFACE:
  subroutine ESMF_ArraySpecSet(arrayspec, rank, typekind, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout)         :: arrayspec
    integer,              intent(in)            :: rank
    type(ESMF_TypeKind),  intent(in)            :: typekind
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates a description of the data -- the typekind, the rank,
! and the dimensionality.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
!   The {\tt ESMF\_ArraySpec} to set.
! \item[rank]
!   Array rank (dimensionality -- 1D, 2D, etc). Maximum allowed is 7D.
! \item[typekind]
!   Array typekind.  See section \ref{opt:typekind} for valid values.
! \item[{[rc]}]
!  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    ! Local vars
    integer :: status ! local error status
    logical :: rcpresent ! did user specify rc?

    ! Initialize pointer
    status = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.

    ! Initialize return code; assume routine is not implemented 
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)

    ! Set arrayspec contents with some checking to keep Silverio at bay
    if (rank.ge.1 .and. rank.le.ESMF_MAXDIM) then
      arrayspec%rank = rank
    else
      ! something to trigger on next time that this is bad
      arrayspec%rank = 0
      if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                            "bad value for rank", &
                             ESMF_CONTEXT, rc)) return
    endif

    ! Since typekind is a derived type, you cannot set it to
    ! illegal values, and no additional validity tests are needed.
    arrayspec%typekind = typekind

    ! set status to indicate that arrayspec has now been defined
    arrayspec%status = ESMF_ARRAYSPEC_STATUS_SET

    ! set return code
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecSet


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecValidate()"
!BOP
! !IROUTINE: ESMF_ArraySpecValidate - Validate ArraySpec internals

! !INTERFACE:
  subroutine ESMF_ArraySpecValidate(arrayspec, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout)              :: arrayspec
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt arrayspec} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[arrayspec] 
!          Specified {\tt ESMF\_ArraySpec} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    !todo: call c_ESMC_ArraySpecValidate(arrayspec, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! check status
    if (arrayspec%status .ne. ESMF_ARRAYSPEC_STATUS_SET) then
       ! Use LogErr to handle return code (to record other info for logfile)
       if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_UNINIT, "ArraySpec hasn't been set", &
                                  ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArraySpecValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecInit()"
!BOPI
! !IROUTINE: ESMF_ArraySpecInit - Init ArraySpec internals

! !INTERFACE:
  subroutine ESMF_ArraySpecInit(arrayspec)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout)              :: arrayspec
!         
!
! !DESCRIPTION:
!      Initialize ArraySpec internals.
!
!     The arguments are:
!     \begin{description}
!     \item[arrayspec] 
!          Specified {\tt ESMF\_ArraySpec} object.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    arrayspec%status = ESMF_ARRAYSPEC_STATUS_NOTSET 

    ESMF_INIT_SET_DEFINED(arrayspec)
  end subroutine ESMF_ArraySpecInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecGetInit"
!BOPI
! !IROUTINE: ESMF_ArraySpecGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_ArraySpecGetInit(arrayspec) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_ArraySpecGetInit   
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in), optional :: arrayspec
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [arrayspec]
!           ArraySpec object.
!     \end{description}
!
!EOPI

    if (present(arrayspec)) then
      ESMF_ArraySpecGetInit = ESMF_INIT_GET(arrayspec)
    else
      ESMF_ArraySpecGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_ArraySpecGetInit
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecStatusEqual"
!BOPI
! !IROUTINE: ESMF_ArraySpecStatusEqual - equality of ArraySpec statuses
!
! !INTERFACE:
      function ESMF_ArraySpecStatusEqual(ArraySpecStatus1, ArraySpecStatus2)

! !RETURN VALUE:
      logical :: ESMF_ArraySpecStatusEqual

! !ARGUMENTS:

      type (ESMF_ArraySpecStatus), intent(in) :: &
         ArraySpecStatus1,      &! Two igrid statuses to compare for
         ArraySpecStatus2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF ArraySpec statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[ArraySpecStatus1, ArraySpecStatus2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_ArraySpecStatusEqual = (ArraySpecStatus1%status == &
                              ArraySpecStatus2%status)

      end function ESMF_ArraySpecStatusEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecStatusNotEqual"
!BOPI
! !IROUTINE: ESMF_ArraySpecStatusNotEqual - non-equality of ArraySpec statuses
!
! !INTERFACE:
      function ESMF_ArraySpecStatusNotEqual(ArraySpecStatus1, ArraySpecStatus2)

! !RETURN VALUE:
      logical :: ESMF_ArraySpecStatusNotEqual

! !ARGUMENTS:

      type (ESMF_ArraySpecStatus), intent(in) :: &
         ArraySpecStatus1,      &! Two ArraySpec Statuses to compare for
         ArraySpecStatus2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF ArraySpec statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[ArraySpecStatus1, ArraySpecStatus2]
!          Two statuses of ArraySpecs to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_ArraySpecStatusNotEqual = (ArraySpecStatus1%status /= &
                                 ArraySpecStatus2%status)

      end function ESMF_ArraySpecStatusNotEqual

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecStatusPrint"
!BOPI
! !IROUTINE: ESMF_ArraySpecStatusPrint - Print information of ArraySpecStatus

! !INTERFACE:
  subroutine ESMF_ArraySpecStatusPrint(arrayspecstatus)
!
! !ARGUMENTS:
    type(ESMF_ArraySpecStatus), intent(in)              :: arrayspecstatus
!         
!
! !DESCRIPTION:
!     Print ArraySpecStatus internals.
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[arrayspecstatus] 
!          Specified {\tt ESMF\_ArraySpecStatus} object.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    write(*, *) "ArraySpecStatus Print Begins =====>"
    write(*, *) "   status = ", arrayspecstatus%status
    write(*, *) "ArraySpecStatus Print Ends   =====>"
    
  end subroutine ESMF_ArraySpecStatusPrint

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecPrint"
!BOP
! !IROUTINE: ESMF_ArraySpecPrint - Print information of ArraySpec

! !INTERFACE:
  subroutine ESMF_ArraySpecPrint(arrayspec, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(in)              :: arrayspec
    integer, intent(out), optional                :: rc
!         
!
! !DESCRIPTION:
!     Print ArraySpec internals.
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[arrayspec] 
!          Specified {\tt ESMF\_ArraySpec} object.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    write(*, *) "ArraySpec Print Begins =====>"
    write(*, *) "   rank = ", arrayspec%rank    
    write(*, *) "   typekind = ", arrayspec%typekind
    call ESMF_ArraySpecStatusPrint(arrayspec%status)
    write(*, *) "ArraySpec Print Ends   =====>"

    rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArraySpecPrint
!------------------------------------------------------------------------------


end module ESMF_ArraySpecMod
