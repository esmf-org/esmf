! $Id: ESMF_VM.F90,v 1.5 2004/03/22 14:55:54 theurich Exp $
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
! ESMF VM Module
module ESMF_VMMod
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
!BOP
! !MODULE: ESMF_VMMod - The VM (virtual machine)
!
! !DESCRIPTION:
!   F90 API wrapper of C++ implemenation of VM and VMPlan
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_BaseMod                          ! ESMF base class
      
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_VM
    sequence
    private
      type(ESMF_Pointer) :: this
  end type

  ! F90 class type to hold pointer to C++ object
  type ESMF_VMPlan
    sequence
    private
      type(ESMF_Pointer) :: this
  end type

!------------------------------------------------------------------------------

  ! Module parameters
  integer, parameter:: ESMF_PREF_INTRA_PROCESS_SHMHACK  = 0 !default
  integer, parameter:: ESMF_PREF_INTRA_PROCESS_PTHREAD  = 1

  integer, parameter:: ESMF_PREF_INTRA_SSI_POSIXIPC     = 0
  integer, parameter:: ESMF_PREF_INTRA_SSI_MPI1         = 1 !default
      
  integer, parameter:: ESMF_PREF_INTER_SSI_MPI1         = 0 !default
  
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_VM
  public ESMF_VMPlan
      
!------------------------------------------------------------------------------
! !PUBLIC PARAMETERS:
      
  public ESMF_PREF_INTRA_PROCESS_SHMHACK
  public ESMF_PREF_INTRA_PROCESS_PTHREAD
  public ESMF_PREF_INTRA_SSI_POSIXIPC
  public ESMF_PREF_INTRA_SSI_MPI1
  public ESMF_PREF_INTER_SSI_MPI1

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PRIVATE MODULE VARIABLES:

  type(ESMF_VM) :: GlobalVM     ! This is a reference to the global VM

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

  ! - ESMF_VM:
  ! For ESMF internal use only
  public ESMF_VMInitialize
  public ESMF_VMFinalize
  ! For ESMF application use
  public ESMF_VMGetGlobal
  public ESMF_VMGet
  public ESMF_VMGetPET
  public ESMF_VMPrint
  ! For advanced ESMF application use
  public ESMF_VMBarrier
  public ESMF_VMThreadBarrier
  public ESMF_VMSend
  public ESMF_VMRecv
  public ESMF_VMScatter
  public ESMF_VMGather
  
  ! - ESMF_VMPlan:
  ! For ESMF internal use only
  public ESMF_VMPlanConstruct
  public ESMF_VMPlanDestruct
  public ESMF_VMPlanMaxThreads
  public ESMF_VMPlanMinThreads
  public ESMF_VMPlanMaxPEs

!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_VM.F90,v 1.5 2004/03/22 14:55:54 theurich Exp $'

!==============================================================================

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMSend -- Generic interface

! !INTERFACE:
      interface ESMF_VMSend

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMSendI4
      module procedure ESMF_VMSendR4
      module procedure ESMF_VMSendR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMSend} functions.   
!EOP 
      end interface

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMScatter -- Generic interface

! !INTERFACE:
      interface ESMF_VMRecv

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMRecvI4
      module procedure ESMF_VMRecvR4
      module procedure ESMF_VMRecvR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMRecv} functions.   
!EOP 
      end interface

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMScatter -- Generic interface

! !INTERFACE:
      interface ESMF_VMScatter

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMScatterI4
      module procedure ESMF_VMScatterR4
      module procedure ESMF_VMScatterR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMScatter} functions.   
!EOP 
      end interface

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGather-- Generic interface

! !INTERFACE:
      interface ESMF_VMGather

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMGatherI4
      module procedure ESMF_VMGatherR4
      module procedure ESMF_VMGatherR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMGather} functions.   
!EOP 
      end interface


!==============================================================================
      

  contains
      
        
!==============================================================================
! ESMF_VM methods:
!==============================================================================


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMInitialize - Initialize the Global VM

! !INTERFACE:
  subroutine ESMF_VMInitialize(rc)
!
! !ARGUMENTS:
    integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Initialize the Global VM
!
!   The arguments are:
!   \begin{description}
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    call c_ESMC_VMInitialize(GlobalVM, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMInitialize error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMInitialize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMFinalize - Finalize Global VM

! !INTERFACE:
  subroutine ESMF_VMFinalize(rc)
!
! !ARGUMENTS:
    integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Finalize Global VM
!
!   The arguments are:
!   \begin{description}
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    call c_ESMC_VMFinalize(status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMFinalize error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMFinalize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGetGlobal - Get Global VM

! !INTERFACE:
  subroutine ESMF_VMGetGlobal(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(out)      :: vm
    integer, intent(out), optional  :: rc           
!
! !DESCRIPTION:
!   Get Global VM
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        Global VM
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! Copy the handle to the global VM into the output variable
    vm = GlobalVM

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMGetGlobal
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGet - Get VM internals

! !INTERFACE:
  subroutine ESMF_VMGet(vm, mypet, npets, npes, mpic, ok_openmp, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    integer,            intent(out), optional :: mypet
    integer,            intent(out), optional :: npets
    integer,            intent(out), optional :: npes
    integer,            intent(out), optional :: mpic
    type(ESMF_Logical), intent(out), optional :: ok_openmp
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get VM internals
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[{[mypet]}]
!        Id of the PET that instantiates the local user code.
!   \item[{[npets]}]
!        Number of PETs in VM.
!   \item[{[npes]}]
!        Number of PEs referenced by VM.
!   \item[{[mpic]}]
!        MPI Intracommunicator for VM.
!   \item[{[ok_openmp]}]
!        Indicate whether user-level OpenMP threading can be supported
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGet(vm, mypet, npets, npes, mpic, ok_openmp, rc)
    
  end subroutine ESMF_VMGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGetPET - Get VM PET internals

! !INTERFACE:
  subroutine ESMF_VMGetPET(vm, petid, npes, ssiid, nthreads, tid, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
    integer,       intent(in)            :: petid
    integer,       intent(out), optional :: npes
    integer,       intent(out), optional :: ssiid
    integer,       intent(out), optional :: nthreads
    integer,       intent(out), optional :: tid
    integer,       intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get VM internals
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[petid] 
!        Id of the PET for which info is queried
!   \item[{[npes]}]
!        Number of PEs in VM.
!   \item[{[ssiid]}]
!        SSI id this PET is running on.
!   \item[{[nthreads]}]
!        Number of PETs in this PET's thread group.
!   \item[{[tid]}]
!        Thread id of this PET.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGetPET(vm, petid, npes, ssiid, nthreads, tid, rc)
 
  end subroutine ESMF_VMGetPET
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMPrint - Print VM internals

! !INTERFACE:
  subroutine ESMF_VMPrint(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)      :: vm
    integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Print VM internals
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    call c_ESMC_VMPrint(vm, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMPrint error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMPrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMBarrier - VM wide barrier

! !INTERFACE:
  subroutine ESMF_VMBarrier(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)      :: vm
    integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide barrier
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    call c_ESMC_VMBarrier(vm, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMBarrier error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMBarrier
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMThreadBarrier - PET thread group wide barrier

! !INTERFACE:
  subroutine ESMF_VMThreadBarrier(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)      :: vm
    integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!   PET thread group wide barrier
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    call c_ESMC_VMThreadBarrier(vm, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMThreadBarrier error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMThreadBarrier
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMSendI4 - VM wide send

! !INTERFACE:
  subroutine ESMF_VMSendI4(vm, message, len, dest, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),         intent(in)            :: vm
    integer(ESMF_KIND_I4), intent(in)            :: message(:)  
    integer,               intent(in)            :: len
    integer,               intent(in)            :: dest
    integer,               intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide send for ESMF_KIND_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[message] 
!        Array holing message data.
!   \item[len] 
!        Number of elements in message
!   \item[dest] 
!        Destination PET
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    size = len * 4 ! 4 bytes
    call c_ESMC_VMSend(vm, message, size, dest, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMSend error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMSendI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMSendR4 - VM wide send

! !INTERFACE:
  subroutine ESMF_VMSendR4(vm, message, len, dest, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R4), intent(in)            :: message(:)  
    integer,            intent(in)            :: len
    integer,            intent(in)            :: dest
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide send for ESMF_KIND_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[message] 
!        Array holing message data.
!   \item[len] 
!        Number of elements in message
!   \item[dest] 
!        Destination PET
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    size = len * 4 ! 4 bytes
    call c_ESMC_VMSend(vm, message, size, dest, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMSend error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMSendR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMSendR8 - VM wide send

! !INTERFACE:
  subroutine ESMF_VMSendR8(vm, message, len, dest, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R8), intent(in)            :: message(:)  
    integer,            intent(in)            :: len
    integer,            intent(in)            :: dest
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide send for ESMF_KIND_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[message] 
!        Array holing message data.
!   \item[len] 
!        Number of elements in message
!   \item[dest] 
!        Destination PET
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    size = len * 8 ! 8 bytes
    call c_ESMC_VMSend(vm, message, size, dest, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMSend error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMSendR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMRecvI4 - VM wide receive

! !INTERFACE:
  subroutine ESMF_VMRecvI4(vm, message, len, source, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),         intent(in)            :: vm
    integer(ESMF_KIND_I4), intent(in)            :: message(:)  
    integer,               intent(in)            :: len
    integer,               intent(in)            :: source
    integer,               intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide receive for ESMF_KIND_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[message] 
!        Array holing message data
!   \item[len] 
!        Number of elements in message
!   \item[source] 
!        Source PET
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    size = len * 4 ! 4 bytes
    call c_ESMC_VMRecv(vm, message, size, source, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMSend error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMRecvI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMRecvR4 - VM wide receive

! !INTERFACE:
  subroutine ESMF_VMRecvR4(vm, message, len, source, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R4), intent(in)            :: message(:)  
    integer,            intent(in)            :: len
    integer,            intent(in)            :: source
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide receive for ESMF_KIND_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[message] 
!        Array holing message data
!   \item[len] 
!        Number of elements in message
!   \item[source] 
!        Source PET
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    size = len * 4 ! 4 bytes
    call c_ESMC_VMRecv(vm, message, size, source, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMSend error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMRecvR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMRecvR8 - VM wide receive

! !INTERFACE:
  subroutine ESMF_VMRecvR8(vm, message, len, source, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R8), intent(in)            :: message(:)  
    integer,            intent(in)            :: len
    integer,            intent(in)            :: source
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide receive for ESMF_KIND_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[message] 
!        Array holing message data
!   \item[len] 
!        Number of elements in message
!   \item[source] 
!        Source PET
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                     ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    size = len * 4 ! 4 bytes
    call c_ESMC_VMRecv(vm, message, size, source, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMSend error"
      return
    endif

    ! Set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMRecvR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMScatterI4 - MPI-like VM wide Scatter

! !INTERFACE:
  subroutine ESMF_VMScatterI4(vm, input, output, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    integer(ESMF_KIND_I4), intent(in)      :: input(:)
    integer(ESMF_KIND_I4), intent(out)     :: output(:)
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Scatter for ESMF_KIND_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holing input data
!   \item[output] 
!        Array holing output data
!   \item[len] 
!        Number of elements send to each PET
!   \item[root] 
!        Root PET id
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    size = len * 4 ! 4 bytes
    call c_ESMC_VMScatter(vm, input, output, size, root, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMScatterI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMScatterR4 - MPI-like VM wide Scatter

! !INTERFACE:
  subroutine ESMF_VMScatterR4(vm, input, output, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R4), intent(in)          :: input(:)
    real(ESMF_KIND_R4), intent(out)         :: output(:)
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Scatter for ESMF_KIND_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holing input data
!   \item[output] 
!        Array holing output data
!   \item[len] 
!        Number of elements send to each PET
!   \item[root] 
!        Root PET id
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    size = len * 4 ! 4 bytes
    call c_ESMC_VMScatter(vm, input, output, size, root, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMScatterR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMScatterR8 - MPI-like VM wide Scatter

! !INTERFACE:
  subroutine ESMF_VMScatterR8(vm, input, output, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R8), intent(in)          :: input(:)
    real(ESMF_KIND_R8), intent(out)         :: output(:)
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Scatter for ESMF_KIND_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holing input data
!   \item[output] 
!        Array holing output data
!   \item[len] 
!        Number of elements send to each PET
!   \item[root] 
!        Root PET id
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    size = len * 8 ! 8 bytes
    call c_ESMC_VMScatter(vm, input, output, size, root, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMScatterR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGatherI4 - MPI-like VM wide Gather

! !INTERFACE:
  subroutine ESMF_VMGatherI4(vm, input, output, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    integer(ESMF_KIND_I4), intent(in)       :: input(:)
    integer(ESMF_KIND_I4), intent(out)      :: output(:)
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Gather for ESMF_KIND_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holing input data
!   \item[output] 
!        Array holing output data
!   \item[len] 
!        Number of elements received from each PET
!   \item[root] 
!        Root PET id
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    size = len * 4 ! 4 bytes
    call c_ESMC_VMGather(vm, input, output, size, root, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMGatherI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGatherR4 - MPI-like VM wide Gather

! !INTERFACE:
  subroutine ESMF_VMGatherR4(vm, input, output, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R4), intent(in)          :: input(:)
    real(ESMF_KIND_R4), intent(out)         :: output(:)
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Gather for ESMF_KIND_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holing input data
!   \item[output] 
!        Array holing output data
!   \item[len] 
!        Number of elements received from each PET
!   \item[root] 
!        Root PET id
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    size = len * 4 ! 4 bytes
    call c_ESMC_VMGather(vm, input, output, size, root, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMGatherR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGatherR8 - MPI-like VM wide Gather

! !INTERFACE:
  subroutine ESMF_VMGatherR8(vm, input, output, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R8), intent(in)          :: input(:)
    real(ESMF_KIND_R8), intent(out)         :: output(:)
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Gather for ESMF_KIND_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holing input data
!   \item[output] 
!        Array holing output data
!   \item[len] 
!        Number of elements received from each PET
!   \item[root] 
!        Root PET id
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: size

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    size = len * 8 ! 8 bytes
    call c_ESMC_VMGather(vm, input, output, size, root, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMGatherR8
!------------------------------------------------------------------------------


!==============================================================================
! ESMF_VMPlan methods:
!==============================================================================


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMPlanConstruct - Construct a default plan

! !INTERFACE:
  subroutine ESMF_VMPlanConstruct(vmplan, vm, npetlist, petlist, rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan), intent(inout)         :: vmplan
    type(ESMF_VM),     intent(in)            :: vm
    integer,           intent(in)            :: npetlist
    integer,           intent(in)            :: petlist(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Construct a default plan
!
!   The arguments are:
!   \begin{description}
!   \item[vmplan] 
!        VMPlan
!   \item[vm] 
!        VM
!   \item[npetlist] 
!        Number of PETs in petlist
!   \item[petlist] 
!        List of PETs that the parent VM will provide to the child VM
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanConstruct(vmplan, vm, npetlist, petlist, rc)

  end subroutine ESMF_VMPlanConstruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMPlanDestruct - Destruct a vmplan

! !INTERFACE:
  subroutine ESMF_VMPlanDestruct(vmplan, rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan), intent(inout)  :: vmplan
    integer, intent(out), optional    :: rc           
!
! !DESCRIPTION:
!   Destruct a vmplan
!
!   The arguments are:
!   \begin{description}
!   \item[vmplan] 
!        VMPlan
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanDestruct(vmplan, rc)

  end subroutine ESMF_VMPlanDestruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMPlanMaxThreads - Set up a MaxThreads vmplan

! !INTERFACE:
  subroutine ESMF_VMPlanMaxThreads(vmplan, vm, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, npetlist, petlist, rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan), intent(inout)         :: vmplan
    type(ESMF_VM),     intent(in)            :: vm
    integer,           intent(in),  optional :: max
    integer,           intent(in),  optional :: pref_intra_process
    integer,           intent(in),  optional :: pref_intra_ssi
    integer,           intent(in),  optional :: pref_inter_ssi
    integer,           intent(in)            :: npetlist
    integer,           intent(in)            :: petlist(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Set up a MaxThreads vmplan
!
!   The arguments are:
!   \begin{description}
!   \item[vmplan] 
!        VMPlan
!   \item[vm] 
!        VM
!   \item[{[max]}] 
!        Maximum threading level
!   \item[{[pref_intra_process]}] 
!        Intra process communication preference
!   \item[{[pref_intra_ssi]}] 
!        Intra SSI communication preference
!   \item[{[pref_inter_ssi]}] 
!        Inter process communication preference
!   \item[npetlist] 
!        Number of PETs in petlist
!   \item[petlist] 
!        List of PETs that the parent VM will provide to the child VM
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, rc)
  
  end subroutine ESMF_VMPlanMaxThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMPlanMinThreads - Set up a MinThreads vmplan

! !INTERFACE:
  subroutine ESMF_VMPlanMinThreads(vmplan, vm, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, npetlist, petlist, rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan), intent(inout)         :: vmplan
    type(ESMF_VM),     intent(in)            :: vm
    integer,           intent(in),  optional :: max
    integer,           intent(in),  optional :: pref_intra_process
    integer,           intent(in),  optional :: pref_intra_ssi
    integer,           intent(in),  optional :: pref_inter_ssi
    integer,           intent(in)            :: npetlist
    integer,           intent(in)            :: petlist(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Set up a MinThreads vmplan
!
!   The arguments are:
!   \begin{description}
!   \item[vmplan] 
!        VMPlan
!   \item[vm] 
!        VM
!   \item[{[max]}] 
!        Maximum number of cores per thread
!   \item[{[pref_intra_process]}] 
!        Intra process communication preference
!   \item[{[pref_intra_ssi]}] 
!        Intra SSI communication preference
!   \item[{[pref_inter_ssi]}] 
!        Inter process communication preference
!   \item[npetlist] 
!        Number of PETs in petlist
!   \item[petlist] 
!        List of PETs that the parent VM will provide to the child VM
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMinThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, rc)
  
  end subroutine ESMF_VMPlanMinThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMPlanMaxPEs - Set up a MaxPEs vmplan

! !INTERFACE:
  subroutine ESMF_VMPlanMaxPEs(vmplan, vm, max, &
    pref_intra_process, pref_intra_ssi, pref_inter_ssi, npetlist, petlist, rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan), intent(inout)         :: vmplan
    type(ESMF_VM),     intent(in)            :: vm
    integer,           intent(in),  optional :: max
    integer,           intent(in),  optional :: pref_intra_process
    integer,           intent(in),  optional :: pref_intra_ssi
    integer,           intent(in),  optional :: pref_inter_ssi
    integer,           intent(in)            :: npetlist
    integer,           intent(in)            :: petlist(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Set up a MaxPEs vmplan
!
!   The arguments are:
!   \begin{description}
!   \item[vmplan] 
!        VMPlan
!   \item[vm] 
!        VM
!   \item[{[max]}] 
!        Maximum number of cores per thread
!   \item[{[pref_intra_process]}] 
!        Intra process communication preference
!   \item[{[pref_intra_ssi]}] 
!        Intra SSI communication preference
!   \item[{[pref_inter_ssi]}] 
!        Inter process communication preference
!   \item[npetlist] 
!        Number of PETs in petlist
!   \item[petlist] 
!        List of PETs that the parent VM will provide to the child VM
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxPEs(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, rc)
  
  end subroutine ESMF_VMPlanMaxPEs
!------------------------------------------------------------------------------


end module ESMF_VMMod
