! $Id: ESMF_VMBase.F90,v 1.1 2004/12/14 15:38:19 theurich Exp $
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
#define ESMF_FILENAME "ESMF_VM.F90"
!==============================================================================
!
! ESMF VM Module
module ESMF_VMBaseMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the VM and VMPlan classes
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_VMMod - The VM (virtual machine)
!
! !DESCRIPTION:
!   F90 API wrapper of C++ implementation of VM and VMPlan
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_BaseTypesMod
  use ESMF_VMTypesMod
      
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
! VM Type and others defined in ESMF_VMTypes.F90

!------------------------------------------------------------------------------
! !PRIVATE MODULE VARIABLES:

! - ESMF-private symbol, but must be public for VM Comm interfaces:
  type(ESMF_VM) :: ESMF_GlobalVM     ! This is a reference to the global VM
  public ESMF_GlobalVM

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_VMGet
  public ESMF_VMGetGlobal
  public ESMF_VMGetPETLocalInfo

  public ESMF_VMPrint

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_VMBase.F90,v 1.1 2004/12/14 15:38:19 theurich Exp $'

!==============================================================================

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================
      

  contains
      
        
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGet()"
!BOP
! !IROUTINE: ESMF_VMGet - Get VM internals

! !INTERFACE:
  subroutine ESMF_VMGet(vm, localPet, petCount, peCount, mpiCommunicator, &
    okOpenMpFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)              :: vm
    integer,            intent(out),  optional  :: localPet
    integer,            intent(out),  optional  :: petCount
    integer,            intent(out),  optional  :: peCount
    integer,            intent(out),  optional  :: mpiCommunicator
    type(ESMF_Logical), intent(out),  optional  :: okOpenMpFlag
    integer,            intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Get internal information about the specified {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        Queried {\tt ESMF\_VM} object.
!   \item[{[localPet]}]
!        Upon return this holds the id of the PET that instantiates the local
!        user code.
!   \item[{[petCount]}]
!        Upon return this holds the number of PETs in the specified 
!        {\tt ESMF\_VM} object.
!   \item[{[peCount]}]
!        Upon return this holds the number of PEs referenced by the specified
!        {\tt ESMF\_VM} object.
!   \item[{[mpiCommunicator]}]
!        Upon return this holds the MPI intra-communicator used by the 
!        specified {\tt ESMF\_VM} object. This communicator may be used for
!        user-level MPI communications. It is recommended that the user
!        duplicates the communicator via {\tt MPI\_Comm\_Dup()} in order to
!        prevent any interference with ESMF communications.
!   \item[{[okOpenMpFlag]}]
!        Upon return this holds a flag indicating whether user-level OpenMP
!        threading is supported by the specified {\tt ESMF\_VM} object.
!        \begin{description}
!        \item[{\tt ESMF\_TRUE}]
!             User-level OpenMP threading is supported.
!        \item[{\tt ESMF\_FALSE}]
!             User-level OpenMP threading is not supported.
!        \end{description}
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGet(vm, localPet, petCount, peCount, mpiCommunicator, &
      okOpenMpFlag, localrc)

    if (present(rc)) rc = localrc

    !! Use LogErr to handle return code
    !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_VMGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMGetGlobal - Get Global VM

! !INTERFACE:
  subroutine ESMF_VMGetGlobal(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(out)            :: vm
    integer,       intent(out), optional  :: rc           
!
! !DESCRIPTION:
!   Get the global default {\tt ESMF\_VM} object. This is the {\tt ESMF\_VM}
!   object that was created during {\tt ESMF\_Initialize()} and is the ultimate
!   parent of all {\tt ESMF\_VM} objects in an ESMF application.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        Upon return this holds the global default {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Copy the handle to the global VM into the output variable
    vm = ESMF_GlobalVM

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMGetGlobal
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGetPETLocalInfo()"
!BOP
! !IROUTINE: ESMF_VMGetPETLocalInfo - Get VM PET local internals

! !INTERFACE:
  subroutine ESMF_VMGetPETLocalInfo(vm, pet, peCount, ssiId, threadCount, &
    threadId, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)              :: vm
    integer,        intent(in)              :: pet
    integer,        intent(out),  optional  :: peCount
    integer,        intent(out),  optional  :: ssiId
    integer,        intent(out),  optional  :: threadCount
    integer,        intent(out),  optional  :: threadId
    integer,        intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Get internal information about the specified PET within the specified
!   {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!         Queried {\tt ESMF\_VM} object.
!   \item[pet] 
!         Queried PET id within the specified {\tt ESMF\_VM} object.
!   \item[{[peCount]}]
!        Upon return this holds the number of PEs associated with the specified
!        PET in the {\tt ESMF\_VM} object.
!   \item[{[ssiId]}]
!        Upon return this holds the id of the single-system image (SSI) the
!        specified PET is running on.
!   \item[{[threadCount]}]
!        Upon return this holds the number of PETs in the specified PET's 
!        thread group.
!   \item[{[threadId]}]
!        Upon return this holds the thread id of the specified PET within the 
!        {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGetPETLocalInfo(vm, pet, peCount, ssiId, threadCount, &
      threadId, localrc)

    if (present(rc)) rc = localrc

    !! Use LogErr to handle return code
    !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_VMGetPETLocalInfo
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPrint()"
!BOP
! !IROUTINE: ESMF_VMPrint - Print VM internals

! !INTERFACE:
  subroutine ESMF_VMPrint(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)              :: vm
    integer,        intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Prints internal information about the specified {\tt ESMF\_VM} to
!   {\tt stdout}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        Specified {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPrint(vm, localrc) 

    if (present(rc)) rc = localrc

    !! Use LogErr to handle return code
    !if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
    !  ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_VMPrint
!------------------------------------------------------------------------------


end module ESMF_VMBaseMod


subroutine f_ESMF_VMGlobalGet(localPet, petCount)
  use ESMF_VMBaseMod
  integer, intent(out), optional  :: localPet
  integer, intent(out), optional  :: petCount
    
  call ESMF_VMGet(ESMF_GlobalVM, localPet=localPet, petCount=petCount)
  
!  if (present(localPet)) localPet = 0
!  if (present(petCount)) petCount = 0
end subroutine f_ESMF_VMGlobalGet
