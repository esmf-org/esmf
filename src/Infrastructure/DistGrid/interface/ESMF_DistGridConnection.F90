! $Id: ESMF_DistGridConnection.F90,v 1.1 2011/06/03 23:33:59 theurich Exp $
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
#define ESMF_FILENAME "ESMF_DistGridConnection.F90"
!==============================================================================
!
! ESMF DistGrid Module
module ESMF_DistGridConnectionMod
!
!==============================================================================
!
! This file contains the DistGridConnection shallow class implementation.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_DistGridConnectionMod
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_F90InterfaceMod        ! ESMF F90-C++ interface helper
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:

  public ESMF_DistGridConnection

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnection()"
!BOP
! !IROUTINE: ESMF_DistGridConnection - Construct a DistGrid connection element
! !INTERFACE:
  subroutine ESMF_DistGridConnection(connection, tileIndexA, tileIndexB, &
    positionVector, keywordEnforcer, orientationVector, rc)
!
! !ARGUMENTS:
    integer,        target, intent(out)           :: connection(:)
    integer,                intent(in)            :: tileIndexA
    integer,                intent(in)            :: tileIndexB
    integer,                intent(in)            :: positionVector(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: orientationVector(:)
    integer,                intent(out), optional :: rc
!         
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!     This call helps to construct a DistGrid connection,
!     which is a simple vector of integers, out of its components.
!
!     The arguments are:
!     \begin{description}
!     \item[connection] 
!        Element to be constructed. The provided {\tt connection} must 
!        be dimensioned to hold exactly the number of integers that result from
!        the input information.
!     \item[tileIndexA] 
!        Index of one of the two tiles that are to be connected.
!     \item[tileIndexB] 
!        Index of one of the two tiles that are to be connected.
!     \item[positionVector] 
!        Position of tile B's minIndex with respect to tile A's minIndex.
!     \item[{[orientationVector]}]
!        Associates each dimension of tile A with a dimension in tile B's 
!        index space. Negative index values may be used to indicate a 
!        reversal in index orientation. It is erroneous to associate multiple
!        dimensions of tile A with the same index in tile B. By default
!        {\tt orientationVector = (/1,2,3,.../)}, i.e. same orientation as
!        tile A.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: connectionArg        ! helper variable
    type(ESMF_InterfaceInt) :: positionVectorArg    ! helper variable
    type(ESMF_InterfaceInt) :: orientationVectorArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Deal with (optional) array arguments
    connectionArg = ESMF_InterfaceIntCreate(connection, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    positionVectorArg = ESMF_InterfaceIntCreate(positionVector, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    orientationVectorArg = ESMF_InterfaceIntCreate(orientationVector, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridConnection(connectionArg, &
      tileIndexA, tileIndexB, positionVectorArg, orientationVectorArg, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(connectionArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(positionVectorArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(orientationVectorArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridConnection
!------------------------------------------------------------------------------


end module ESMF_DistGridConnectionMod
