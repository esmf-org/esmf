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
!
#define ESMF_FILENAME "ESMF_StaggerLocType.F90"
!
!     ESMF Stagger Location
      module ESMF_StaggerLocTypeMod
!
!==============================================================================
!
! This file contains the Stagger Location subroutines
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_StaggerMod - Stagger class
!
! !DESCRIPTION:
!
! The code in this file implements some routines for interacting with the general stagger.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_LogErrMod
      use ESMF_IOUtilMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StaggerLoc
!

      type ESMF_StaggerLoc
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
        integer :: staggerloc
      end type



!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
  public ESMF_StaggerLoc

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
  public assignment(=)
  public operator(==), operator(/=) 
  public operator(>), operator(>=) 
  public operator(<), operator(<=) 

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
! 
! For definition of stagger locations see the file Grid\_options.tex.
!
!!!!!

   !! Invalid stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_INVALID       = ESMF_StaggerLoc(-2), &  
      ESMF_STAGGERLOC_UNINIT        = ESMF_StaggerLoc(-1)   

   !! 2D predefined stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_CENTER        = ESMF_StaggerLoc( 0), &     
      ESMF_STAGGERLOC_EDGE1         = ESMF_StaggerLoc( 1), &  
      ESMF_STAGGERLOC_EDGE2         = ESMF_StaggerLoc( 2), &
      ESMF_STAGGERLOC_CORNER        = ESMF_StaggerLoc( 3)

   !! 3D predefined stagger locations
   type (ESMF_StaggerLoc), parameter, public ::            &
      ESMF_STAGGERLOC_CENTER_VCENTER  = ESMF_StaggerLoc( 0), &
      ESMF_STAGGERLOC_EDGE1_VCENTER   = ESMF_StaggerLoc( 1), &
      ESMF_STAGGERLOC_EDGE2_VCENTER   = ESMF_StaggerLoc( 2), &
      ESMF_STAGGERLOC_CORNER_VCENTER  = ESMF_StaggerLoc( 3), &
      ESMF_STAGGERLOC_CENTER_VFACE    = ESMF_StaggerLoc( 4), &
      ESMF_STAGGERLOC_EDGE1_VFACE     = ESMF_StaggerLoc( 5), &
      ESMF_STAGGERLOC_EDGE2_VFACE     = ESMF_StaggerLoc( 6), &
      ESMF_STAGGERLOC_CORNER_VFACE    = ESMF_StaggerLoc( 7)


!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'


!==============================================================================


!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface assignment (=)
         module procedure ESMF_StaggerLocAssignment

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface assigns a string value to an ESMF_StaggerLoc
!
!EOPI
         module procedure ESMF_StaggerLocToInt
         module procedure ESMF_IntToStaggerLoc
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
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
         module procedure ESMF_StaggerLocNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (>)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocGreater

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (<)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocLess

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (>=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocGreaterEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (<=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_StaggerLocLessEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF StaggerLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface

!
!==============================================================================

      contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocAssignment"
!BOPI
! !IROUTINE: ESMF\_StaggerLocAssignment - Assignment of an ESMF\_StaggerLoc from a string
!
! !INTERFACE:
      subroutine ESMF_StaggerLocAssignment(slval, string)

! !ARGUMENTS:
      type(ESMF_StaggerLoc), intent(out) :: slval 
      character(len=*), intent(in) :: string

! !DESCRIPTION:
!     This routine assigns an ESMF\_StaggerLoc from a string value
!
!     The arguments are:
!     \begin{description}
!     \item[string]
!       String value to assign to the ESMF\_StaggerLoc
!     \end{description}
!
!EOPI


      if (string == "ESMF_STAGGERLOC_INVALID") then
        slval = ESMF_STAGGERLOC_INVALID
      else if (string == "ESMF_STAGGERLOC_UNINIT") then
        slval = ESMF_STAGGERLOC_UNINIT
      else if (string == "ESMF_STAGGERLOC_CENTER") then
        slval = ESMF_STAGGERLOC_CENTER
      else if (string == "ESMF_STAGGERLOC_EDGE1") then
        slval = ESMF_STAGGERLOC_EDGE1
      else if (string == "ESMF_STAGGERLOC_EDGE2") then
        slval = ESMF_STAGGERLOC_EDGE2
      else if (string == "ESMF_STAGGERLOC_CORNER") then
         slval = ESMF_STAGGERLOC_CORNER
      endif

      end subroutine ESMF_StaggerLocAssignment
      
subroutine ESMF_StaggerLocToInt(lhsInt, rhsStaggerLoc)
  integer,                   intent(out) :: lhsInt
  type(ESMF_StaggerLoc),     intent(in)  :: rhsStaggerLoc
  lhsInt = rhsStaggerLoc%staggerloc
end subroutine

subroutine ESMF_IntToStaggerLoc(lhsStaggerLoc, rhsInt)
  type(ESMF_StaggerLoc),     intent(out) :: lhsStaggerLoc
  integer,                   intent(in)  :: rhsInt
  lhsStaggerLoc = ESMF_StaggerLoc(rhsInt)
end subroutine
      
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocEqual"
!BOPI
! !IROUTINE: ESMF_StaggerLocEqual - Equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocEqual

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two igrid statuses to compare for
         StaggerLoc2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocEqual = (StaggerLoc1%staggerloc == &
                              StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocNotEqual"
!BOPI
! !IROUTINE: ESMF_StaggerLocNotEqual - Non-equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocNotEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocNotEqual
 
! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two StaggerLoc Statuses to compare for
         StaggerLoc2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two statuses of StaggerLocs to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocNotEqual = (StaggerLoc1%staggerloc /= &
                                 StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocNotEqual


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocGreater"
!BOPI
! !IROUTINE: ESMF_StaggerLocGreater - Equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocGreater(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocGreater

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two igrid statuses to compare for
         StaggerLoc2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
 !     \item[StaggerLoc1, StaggerLoc2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocGreater = (StaggerLoc1%staggerloc > &
                              StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocGreater
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocLess"
!BOPI
! !IROUTINE: ESMF_StaggerLocLess - Non-equality of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocLess(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocLess

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two StaggerLoc Statuses to compare for
         StaggerLoc2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two statuses of StaggerLocs to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_StaggerLocLess = (StaggerLoc1%staggerloc < &
                                 StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocLess

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocGreaterEqual"
!BOPI
 ! !IROUTINE: ESMF_StaggerLocGreaterEqual - Greater than or equal of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocGreaterEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocGreaterEqual

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two igrid statuses to compare for
         StaggerLoc2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two igrid statuses to compare
!     \end{description}
!
!EOPI

      ESMF_StaggerLocGreaterEqual = (StaggerLoc1%staggerloc >= &
                              StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocGreaterEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StaggerLocLessEqual"
!BOPI
! !IROUTINE: ESMF_StaggerLocLessEqual - Less than or equal of StaggerLoc statuses
!
! !INTERFACE:
      function ESMF_StaggerLocLessEqual(StaggerLoc1, StaggerLoc2)

! !RETURN VALUE:
      logical :: ESMF_StaggerLocLessEqual

! !ARGUMENTS:

      type (ESMF_StaggerLoc), intent(in) :: &
         StaggerLoc1,      &! Two StaggerLoc Statuses to compare for
         StaggerLoc2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF StaggerLoc statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[StaggerLoc1, StaggerLoc2]
!          Two statuses of StaggerLocs to compare
!     \end{description}
!
!EOPI

      ESMF_StaggerLocLessEqual = (StaggerLoc1%staggerloc <= &
                                 StaggerLoc2%staggerloc)

      end function ESMF_StaggerLocLessEqual

      end module ESMF_StaggerLocTypeMod

