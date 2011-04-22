! $Id: ESMF_ArraySpec.F90,v 1.48 2011/04/22 16:55:57 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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
! ! ESMF_ArraySpec
!
! ! Data array specification, with no associated data buffer.
  type ESMF_ArraySpec
    sequence
    private
    integer             :: rank       ! number of dimensions
    type(ESMF_TypeKind) :: typekind   ! fortran type and kind enum/integer
    ESMF_INIT_DECLARE
  end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_ArraySpec

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_ArraySpecGet
  public ESMF_ArraySpecPrint
  public ESMF_ArraySpecSet
  public ESMF_ArraySpecValidate

! - ESMF-internal methods:
  public ESMF_ArraySpecInit
  public ESMF_ArraySpecGetInit

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_ArraySpec.F90,v 1.48 2011/04/22 16:55:57 theurich Exp $'

!==============================================================================

#if 0
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_ArraySpecEQ

! !DESCRIPTION:
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_ArraySpecNE

! !DESCRIPTION:
!
!EOPI
      end interface
!
!==============================================================================
#endif

  contains

!==============================================================================


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecGet"
!BOP
! !IROUTINE: ESMF_ArraySpecGet - Get values from an ArraySpec
!
! !INTERFACE:
  subroutine ESMF_ArraySpecGet(arrayspec, keywordEnforcer, rank, typekind, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(in)            :: arrayspec
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: rank
    type(ESMF_TypeKind),  intent(out), optional :: typekind
    integer,              intent(out), optional :: rc
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Returns information about the contents of an {\tt ESMF\_ArraySpec}.
!
!   The arguments are:
!   \begin{description}
!   \item[arrayspec]
!     The {\tt ESMF\_ArraySpec} to query.
!   \item[{[rank]}]
!     Array rank (dimensionality -- 1D, 2D, etc). Maximum possible is 7D.
!   \item[{[typekind]}]
!     Array typekind.  See section \ref{opt:typekind} for valid values.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, arrayspec, rc)

    ! Get arrayspec contents
    if(present(rank)) rank = arrayspec%rank
    if(present(typekind)) typekind = arrayspec%typekind

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecPrint"
!BOP
! !IROUTINE: ESMF_ArraySpecPrint - Print information of ArraySpec

! !INTERFACE:
  subroutine ESMF_ArraySpecPrint(arrayspec, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(in)              :: arrayspec
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional   :: rc
!         
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!     Print ArraySpec internals. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!     may be used on unit 6 to get coherent output.  \\
!
!     The arguments are:
!     \begin{description}
!     \item[arrayspec] 
!         Specified {\tt ESMF\_ArraySpec} object.
!     \item[{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, arrayspec, rc)

    write(*, *) "ArraySpec Print Begins =====>"
    write(*, *) "   rank = ", arrayspec%rank    
    write(*, *) "   typekind = ", arrayspec%typekind
    write(*, *) "ArraySpec Print Ends   =====>"

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecSet"
!BOP
! !IROUTINE: ESMF_ArraySpecSet - Set values for an ArraySpec
!
! !INTERFACE:
  subroutine ESMF_ArraySpecSet(arrayspec, rank, typekind, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(out)           :: arrayspec
    integer,              intent(in)            :: rank
    type(ESMF_TypeKind),  intent(in)            :: typekind
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out), optional :: rc
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Creates a description of the data -- the typekind, the rank,
!   and the dimensionality.
!
!   The arguments are:
!   \begin{description}
!   \item[arrayspec]
!     The {\tt ESMF\_ArraySpec} to set.
!   \item[rank]
!     Array rank (dimensionality -- 1D, 2D, etc). Maximum allowed is 7D.
!   \item[typekind]
!     Array typekind.  See section \ref{opt:typekind} for valid values.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(arrayspec)

    ! set rank
    arrayspec%rank = rank
    if (rank < 1 .or. rank > ESMF_MAXDIM) then
      ! not a valid rank value
      if (ESMF_LogFoundError(ESMF_RC_OBJ_BAD, &
        msg="bad value for rank", &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
    endif

    ! set typekind (do not need check because parameterized type)
    arrayspec%typekind = typekind
    
    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(arrayspec)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecSet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecValidate()"
!BOP
! !IROUTINE: ESMF_ArraySpecValidate - Validate ArraySpec internals

! !INTERFACE:
  subroutine ESMF_ArraySpecValidate(arrayspec, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(in)              :: arrayspec
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,              intent(out),  optional  :: rc  
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Validates that the {\tt arrayspec} is internally consistent.
!   The method returns an error code if problems are found.  
!
!   The arguments are:
!   \begin{description}
!   \item[arrayspec] 
!     Specified {\tt ESMF\_ArraySpec} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, arrayspec, rc)

    ! return successfully
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
    type(ESMF_ArraySpec), intent(out)             :: arrayspec
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
!------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------
    if (present(arrayspec)) then
      ESMF_ArraySpecGetInit = ESMF_INIT_GET(arrayspec)
    else
      ESMF_ArraySpecGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_ArraySpecGetInit
!------------------------------------------------------------------------------


end module ESMF_ArraySpecMod
