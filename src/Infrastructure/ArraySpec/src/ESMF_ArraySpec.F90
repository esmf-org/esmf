! $Id: ESMF_ArraySpec.F90,v 1.18 2007/02/19 23:44:40 rosalind Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
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
    integer             :: rank   ! number of dimensions
    type(ESMF_DataType) :: type   ! real/float, integer, etc enum
    type(ESMF_TypeKind) :: kind   ! fortran "kind" enum/integer
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

! - ESMF-private methods:
  public ESMF_ArraySpecInit
  public ESMF_ArraySpecGetInit

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_ArraySpec.F90,v 1.18 2007/02/19 23:44:40 rosalind Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArraySpecSet - Set the contents of an ArraySpec 
!
! !INTERFACE:
  interface ESMF_ArraySpecSet
  
! !PRIVATE MEMBER FUNCTIONS:
    module procedure ESMF_ArraySpecSetThree
    module procedure ESMF_ArraySpecSetTwo

! !DESCRIPTION:
!     This interface provides an entry point for methods that sets
!     an {\tt ESMF\_ArraySpec}.

  end interface
!EOPI
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
  subroutine ESMF_ArraySpecGet(arrayspec, rank, type, kind, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout)            :: arrayspec
    integer,              intent(out), optional :: rank
    type(ESMF_DataType),  intent(out), optional :: type
    type(ESMF_TypeKind),  intent(out), optional :: kind
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
!   {\tt ESMF\_Array} rank (dimensionality -- 1D, 2D, etc). Maximum
!    possible is 7D.
! \item[type]
!  {\tt ESMF\_Array} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
!  {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
!  {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
!  {\tt ESMF\_Array} kind. Valid kinds include {\tt ESMF\_I4},
!  {\tt ESMF\_I8}, {\tt ESMF\_R4}, {\tt ESMF\_R8},
!  {\tt ESMF\_C8}, {\tt ESMF\_C16}.
! \item[[rc]]
!  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    ! Local vars
    integer :: status ! local error status
    logical :: rcpresent ! did user specify rc?

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_FAILURE
    endif
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)

    ! Get arrayspec contents
    if(present(rank)) rank = arrayspec%rank
    if(present(type)) type = arrayspec%type
    if(present(kind)) kind = arrayspec%kind

    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecSetThree"
!BOP
! !IROUTINE: ESMF_ArraySpecSet - Set values for an ArraySpec using type,kind,rank
!
! !INTERFACE:
     ! Private name; call using ESMF_ArraySpecSet()
  subroutine ESMF_ArraySpecSetThree(arrayspec, rank, type, kind, rc)
!
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout)         :: arrayspec
    integer,              intent(in)            :: rank
    type(ESMF_DataType),  intent(in)            :: type
    type(ESMF_TypeKind),  intent(in)            :: kind
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates a description of the data -- the type, the dimensionality, etc.
! This specification can be used in an {\tt ESMF\_ArrayCreate} call with
! data to create a full {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
!  The {\tt ESMF\_ArraySpec} to set.
! \item[rank]
!  Array rank (dimensionality -- 1D, 2D, etc). Maximum allowed is 7D.
! \item[type]
!  {\tt ESMF\_Array} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
!  {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
!  {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
!  {\tt ESMF\_Array} kind. Valid kinds include {\tt ESMF\_I4},
!  {\tt ESMF\_I8}, {\tt ESMF\_R4}, {\tt ESMF\_R8},
!  {\tt ESMF\_C8}, {\tt ESMF\_C16}.
! \item[{[rc]}]
!  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

    ! Local vars
    integer :: status ! local error status
    logical :: rcpresent ! did user specify rc?

    ! Initialize pointer
    status = ESMF_FAILURE
    rcpresent = .FALSE.

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_FAILURE
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

    ! Since type and kind are derived types, you cannot set them to
    ! illegal values, so no additional tests are needed.
    arrayspec%type = type
    arrayspec%kind = kind

    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecSetThree

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArraySpecSetTwo"
!BOPI
! !IROUTINE: ESMF_ArraySpecSet - Set values for an ArraySpec using type and kind
!
! !INTERFACE:
     ! Private name; call using ESMF_ArraySpecSet()
  subroutine ESMF_ArraySpecSetTwo(arrayspec, rank, typekind, rc)
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout)         :: arrayspec
    integer,              intent(in)            :: rank
    type(ESMF_TypeKind),  intent(in)            :: typekind
    integer,              intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates a description of the data -- the type, the dimensionality, etc.
! This internal version allows the type and kind to be specified as a single
! argument, which eases the depth of the nesting of case statements in
! handling all possible combination of arguments.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
!   The {\tt ESMF\_ArraySpec} to set.
! \item[rank]
!   Array rank (dimensionality -- 1D, 2D, etc). Maximum allowed is 7D.
! \item[typekind]
!  {\tt ESMF\_Array} kind. Valid kinds include 
!  {\tt ESMF\_I1}, {\tt ESMF\_I2}, {\tt ESMF\_I4}, {\tt ESMF\_I8}, 
!  {\tt ESMF\_R4}, {\tt ESMF\_R8}, {\tt ESMF\_C8}, {\tt ESMF\_C16}.
! \item[{[rc]}]
!  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

    ! Local vars
    integer :: status ! local error status
    logical :: rcpresent ! did user specify rc?

    ! Initialize pointer
    status = ESMF_FAILURE
    rcpresent = .FALSE.

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_FAILURE
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

    ! Since type and kind are derived types, you cannot set them to
    ! illegal values, so no additional validity tests are needed.
    arrayspec%kind = typekind
    select case (typekind%dkind)
#ifndef ESMF_NO_INTEGER_1_BYTE 
    case (ESMF_TYPEKIND_I1%dkind)
      arrayspec%type = ESMF_DATA_INTEGER
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
    case (ESMF_TYPEKIND_I2%dkind)
      arrayspec%type = ESMF_DATA_INTEGER
#endif
    case (ESMF_TYPEKIND_I4%dkind)
      arrayspec%type = ESMF_DATA_INTEGER
    case (ESMF_TYPEKIND_I8%dkind)
      arrayspec%type = ESMF_DATA_INTEGER
    case (ESMF_TYPEKIND_R4%dkind)
      arrayspec%type = ESMF_DATA_REAL
    case (ESMF_TYPEKIND_R8%dkind)
      arrayspec%type = ESMF_DATA_REAL
    case (ESMF_C8%dkind)
      arrayspec%type = ESMF_DATA_COMPLEX
    case (ESMF_C16%dkind)
      arrayspec%type = ESMF_DATA_COMPLEX
    end select

    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_ArraySpecSetTwo


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
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    !todo: call c_ESMC_ArraySpecValidate(arrayspec, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_ArraySpecValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
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
    ESMF_INIT_SET_DEFINED(arrayspec)
  end subroutine ESMF_ArraySpecInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
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


end module ESMF_ArraySpecMod
