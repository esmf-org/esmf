! $Id: ESMF_Fraction.F90,v 1.11 2004/12/17 22:35:44 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Fraction Module
!
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
      use ESMF_BaseTypesMod
      use ESMF_BaseMod
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
      sequence
      private
        integer :: whole        ! Integer (whole) seconds (signed)
        integer :: numerator    ! Integer fraction (exact) n/d; numerator (signed)
        integer :: denominator  ! Integer fraction (exact) n/d; denominator
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Fraction
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! !PRIVATE MEMBER FUNCTIONS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Fraction.F90,v 1.11 2004/12/17 22:35:44 eschwab Exp $'

!==============================================================================

      contains

!==============================================================================
!
! Wrappers to C++ fraction routines
!
!------------------------------------------------------------------------------
!

    ! dummy entry point to make ranlib stop complaining about a file
    ! with no symbols.  TODO: replace with real code asap.   nsc.
    subroutine ESMF_FractionDummy(rc)
       integer :: rc

       rc = ESMF_FAILURE
    end subroutine ESMF_FractionDummy

!------------------------------------------------------------------------------

      end module ESMF_FractionMod
