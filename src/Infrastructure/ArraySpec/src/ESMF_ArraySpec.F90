! $Id: ESMF_ArraySpec.F90,v 1.3 2004/03/22 19:02:30 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
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
      use ESMF_BaseMod
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
      !!private

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
      '$Id: ESMF_ArraySpec.F90,v 1.3 2004/03/22 19:02:30 cdeluca Exp $'


!==============================================================================

      contains

!==============================================================================

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
! Return information about the contents of a {\tt ESMF\_ArraySpec} type.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
! An {\tt ESMF\_ArraySpec} object.
! \item[rank]
! {\tt ESMF\_Array} rank (dimensionality, 1D, 2D, etc). Maximum
! allowed is 7D.
! \item[type]
! {\tt ESMF\_Array} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
! {\tt ESMF\_Array} kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP

        ! Local vars
        integer :: i
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
!BOP
! !IROUTINE: ESMF_ArraySpecSet - Set values for an ArraySpec
!
! !INTERFACE:
     subroutine ESMF_ArraySpecSet(arrayspec, rank, type, kind, rc)
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
! Uninitialized array spec.
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 7D.
! \item[type]
! {\tt ESMF\_Array} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
! {\tt ESMF\_Array} kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
          print *, "ERROR in ESMF_ArraySpecSet: bad rank"
          ! something to trigger on next time that this is bad
          arrayspec%rank = 0
          return
        endif

        ! Since type and kind are derived types, you cannot set them to
        ! illegal values, so no additional tests are needed.
        arrayspec%type = type
        arrayspec%kind = kind

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArraySpecSet


        end module ESMF_ArraySpecMod
