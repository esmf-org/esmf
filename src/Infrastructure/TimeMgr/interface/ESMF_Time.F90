! $Id: ESMF_Time.F90,v 1.2 2002/10/21 20:10:28 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Time Module
!
! (all lines below between the !BOP and !EOP markers will be included in
!  the automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_TimeMgr.h>

!------------------------------------------------------------------------------
! module definition

      module ESMF_TimeMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_TimeMod - Base ESMF time definition 
!
! !DESCRIPTION:
!     Part of Time Manager F90 API wrapper of C++ implemenation
!
!     This module serves only as the common Time definition inherited
!     by ESMF\_TimeInterval and ESMF\_TimeInstant
!
! !USES:
        use ESMF_BaseLiteMod     ! ESMF lightweight Base class
        use ESMF_TypesMod        ! ESMF platform independent types
        implicit none
!
! !PRIVATE TYPES:
        private

        type ESMF_Time
            private
            sequence                    ! for C++ interoperability
                integer(INT64) :: S     ! whole seconds
                integer(INT32) :: Sn    ! fractional seconds, numerator
                integer(INT32) :: Sd    ! fractional seconds, denominator
        end type
!
! !PUBLIC TYPES:
        public ESMF_Time
!
! !PUBLIC MEMBER FUNCTIONS:
!        None exposed at F90 API layer; inherited through ESMF\_TimeInterval
!        and ESMF\_TimeInstant
!
!EOP
!===============================================================================

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: ESMF_Time.F90,v 1.2 2002/10/21 20:10:28 eschwab Exp $'
!------------------------------------------------------------------------------

    end module ESMF_TimeMod
