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
#define ESMF_FILENAME "ESMF_Fraction.F90"
!==============================================================================
!
!     ESMF Fraction Module
      module ESMF_FractionMod
!
!==============================================================================
!
! This file contains the Fraction class definition and all Fraction
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!===============================================================================
!BOPI
!
! !MODULE: ESMF_FractionMod
!
! !DESCRIPTION:
! Part of ESMF Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ implementaion of class {\tt ESMC\_Fraction}.
!
! See {\tt ../include/ESMC\_Fraction.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Fraction
!
!     ! Fortran class type to match C++ Fraction class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type ESMF_Fraction
#ifndef ESMF_NO_SEQUENCE
      sequence
#endif
      private  ! (members opaque on Fortran side)
        integer(ESMF_KIND_I8) :: shallowMemory(3)
        ESMF_INIT_DECLARE
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Fraction
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_FractionGetInit
      public ESMF_FractionInit
      public ESMF_FractionValidate

! !PRIVATE MEMBER FUNCTIONS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FractionGetInit"
!BOPI
! !IROUTINE:  ESMF_FractionGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_FractionGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_Fraction), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_FractionGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt fraction}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Fraction} from which to retrieve status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_FractionGetInit = ESMF_INIT_GET(s)
       else
         ESMF_FractionGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_FractionGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FractionInit"
!BOPI
! !IROUTINE:  ESMF_FractionInit - Initialize Fraction

! !INTERFACE:
    subroutine ESMF_FractionInit(s)
!
! !ARGUMENTS:
       type(ESMF_Fraction) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt fraction}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Fraction} of which being initialized.
!     \end{description}
!
!EOPI

        ESMF_INIT_SET_DEFINED(s)

    end subroutine ESMF_FractionInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FractionValidate"
!BOPI
! !IROUTINE:  ESMF_FractionValidate - Check validity of a Fraction

! !INTERFACE:
    subroutine ESMF_FractionValidate(s, rc)
!
! !ARGUMENTS:
       type(ESMF_Fraction), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt Fraction} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Fraction} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt fraction}
!           is valid.
!     \end{description}
!
!EOPI
     ESMF_INIT_CHECK_SET_SHALLOW(ESMF_FractionGetInit,ESMF_FractionInit,s)

     !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
     !TODO: Remove the following dummy test when implementing this method
     if (s%shallowMemory(1)==s%shallowMemory(1)) continue

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_FractionValidate


!==============================================================================
!
! Wrappers to C++ fraction routines
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

      end module ESMF_FractionMod
