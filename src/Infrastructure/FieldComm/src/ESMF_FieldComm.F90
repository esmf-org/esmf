! $Id: ESMF_FieldComm.F90,v 1.100 2008/05/01 16:53:25 feiliu Exp $
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
    private

!  <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!  <none>

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_FieldGather         ! Combine 1 decomposed Field into 1 copy on 1 DE
!
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id: ESMF_FieldComm.F90,v 1.100 2008/05/01 16:53:25 feiliu Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!==============================================================================
!
     contains

#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldGather"
!BOP
! !IROUTINE: ESMF_FieldGather - Data gather operation on a Field

! !INTERFACE:
    subroutine ESMF_FieldGather(field, dstPET, array, patch, vm, rc)
!
!
! !ARGUMENTS:
    type(ESMF_Field), intent(inout)                   :: field
    integer, intent(in)                               :: dstPET
    type(ESMF_Array), intent(out)                     :: array
    integer, intent(in), optional                     :: patch         
    type(ESMF_VM), intent(in), optional               :: vm
    integer, intent(out), optional                    :: rc
!
! !DESCRIPTION:
!     Collect all local data associated with a distributed {\tt ESMF\_Field}
!     into a new {\tt ESMF\_Array} which is created only on a single PET.
!     This routine must be called collectively, that is, on all PETs in
!     an {\tt ESMF\_VM}.  The framework will create a new
!     {\tt ESMF\_Array} to hold the resulting data only on the specified
!     destination PET.  After this call returns the {\tt array} argument
!     will be valid only on the {\tt dstPET} and invalid on all other PETs.
!     The input {\tt field} will be unchanged; the routine creates a copy of
!     the collected data.
!
!     The arguments are:
!     \begin{description}
!     \item [field]
!           {\tt ESMF\_Field} containing data to be gathered.
!     \item [dstPET]
!           Destination PET number where the gathered data is to be returned.
!     \item [array]
!           Newly created {\tt ESMF\_Array} containing the collected data on
!           the specified PET.  It is the size of the entire undecomposed igrid.
!           On all other PETs this argument returns an invalid object.
!           Note that the user should not create an {\tt ESMF\_Array} before
!           making this call; the {\tt ESMF\_Array} should be an uninitialized
!           variable.  When this routine returns, there will be a valid
!           {\tt ESMF\_Array} only on
!           the specified PET number, so code which will access the
!           {\tt ESMF\_Array} should check the current PET number and only
!           try to access it from a single PET.
!     \item [{[patch]}]
!           For multi-patch distgrid, specify the patch of the Field to
!           collect the data from. Default to 1.
!     \item [{[vm]}]
!           {\tt ESMF\_VM} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

    ! local variables
    integer                         :: localrc    ! Error status
    type(ESMF_FieldType), pointer   :: ftypep     ! field type info

    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check variable
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)

    ftypep => field%ftypep

    !if (ESMF_LogMsgFoundError(status, &
    !                            ESMF_ERR_PASSTHRU, &
    !                            ESMF_CONTEXT, rc)) return

    ! Set return values.
    if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_FieldGather
!
!==============================================================================
!------------------------------------------------------------------------------

end module ESMF_FieldCommMod
