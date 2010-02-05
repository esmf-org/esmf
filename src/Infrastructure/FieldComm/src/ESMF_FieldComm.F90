! $Id: ESMF_FieldComm.F90,v 1.102.4.1 2010/02/05 19:56:47 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_FieldComm.F90"
!
!     ESMF Field Communications module
module ESMF_FieldCommMod
!
!==============================================================================
!
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldCommMod - Communication routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Field} class
! communication routines, including Redistribution, Halo, Gather,
! Scatter, and others.
!
! This type is implemented in Fortran 90 and a corresponding
! C++ interface is provided for access.
!
!------------------------------------------------------------------------------
! !USES:
    use ESMF_UtilTypesMod
    use ESMF_InitMacrosMod
    use ESMF_LogErrMod
    use ESMF_BaseMod
    use ESMF_IOSpecMod
    use ESMF_VMMod
    use ESMF_DELayoutMod
    use ESMF_RHandleMod
    use ESMF_FieldMod
    use ESMF_ArrayMod
    implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldComm.F90,v 1.102.4.1 2010/02/05 19:56:47 svasquez Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!==============================================================================
!
!     contains
!
!==============================================================================
!------------------------------------------------------------------------------

end module ESMF_FieldCommMod
