! $Id: ESMF_ArraySpec.F90,v 1.11.2.4 2007/10/18 02:42:19 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_ArraySpec.F90"
!
! ESMF ArraySpec module
      module ESMF_ArraySpecMod
!
!==============================================================================
!
! This file contains the ArraySpec class definition and all ArraySpec
! class methods.
!

#include "ESMF.h"

!------------------------------------------------------------------------------
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
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
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

        integer :: rank             ! number of dimensions
        type(ESMF_DataType) :: type ! real/float, integer, etc enum
        type(ESMF_DataKind) :: kind ! fortran "kind" enum/integer

      end type


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_ArraySpec

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArraySpecSet
      public ESMF_ArraySpecGet

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_ArraySpec.F90,v 1.11.2.4 2007/10/18 02:42:19 cdeluca Exp $'

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
      type(ESMF_ArraySpec), intent(in) :: arrayspec
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
      integer, intent(out), optional :: rc
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

        ! Get arrayspec contents

        if(present(rank)) rank = arrayspec%rank
        if(present(type)) type = arrayspec%type
        if(present(kind)) kind = arrayspec%kind

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
     type(ESMF_ArraySpec), intent(inout) :: arrayspec
     integer, intent(in) :: rank
     type(ESMF_DataType), intent(in) :: type
     type(ESMF_DataKind), intent(in) :: kind
     integer, intent(out), optional :: rc
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
     type(ESMF_ArraySpec), intent(inout) :: arrayspec
     integer, intent(in) :: rank
     type(ESMF_DataKind), intent(in) :: typekind
     integer, intent(out), optional :: rc
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
          case (ESMF_I1%dkind)
            arrayspec%type = ESMF_DATA_INTEGER
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE 
          case (ESMF_I2%dkind)
            arrayspec%type = ESMF_DATA_INTEGER
#endif
          case (ESMF_I4%dkind)
            arrayspec%type = ESMF_DATA_INTEGER
          case (ESMF_I8%dkind)
            arrayspec%type = ESMF_DATA_INTEGER
          case (ESMF_R4%dkind)
            arrayspec%type = ESMF_DATA_REAL
          case (ESMF_R8%dkind)
            arrayspec%type = ESMF_DATA_REAL
          case (ESMF_C8%dkind)
            arrayspec%type = ESMF_DATA_COMPLEX
          case (ESMF_C16%dkind)
            arrayspec%type = ESMF_DATA_COMPLEX
         end select

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArraySpecSetTwo


        end module ESMF_ArraySpecMod
