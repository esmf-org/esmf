! $Id: ESMF_VM.F90,v 1.15 2004/05/19 02:16:07 theurich Exp $
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
!BOPI
! !MODULE: ESMF_VMMod - The VM (virtual machine)
!
! !DESCRIPTION:
!   F90 API wrapper of C++ implementation of VM and VMPlan
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_BaseMod                          ! ESMF base class
      
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_CommHandle
!
! TODO: This needs to be filled with life once we work on non-blocking 
!     
!     ! Shallow sync/async communications type.  Mirrored on C++ side.
!     ! Contains a place to hold
!     ! the MPI handle in the case of nonblocking MPI calls.  The wait
!     ! parameter controls whether the "IsComplete" call blocks/waits
!     ! or simply tests and returns.
      
      type ESMF_CommHandle
      sequence
      private
        integer :: dummy  !so compiler is satisfied for now...
!        integer :: mpi_handle  ! mpi returns this for async calls
!        integer :: wait        ! after an async call, does query block?
      end type
      
      integer, parameter :: ESMF_TEST_COMPLETE = 1, ESMF_WAIT_COMPLETE = 2

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
  public ESMF_CommHandle
  public ESMF_VM
  public ESMF_VMPlan
      
!------------------------------------------------------------------------------
! !PUBLIC PARAMETERS:
      
  public ESMF_TEST_COMPLETE, ESMF_WAIT_COMPLETE
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
  ! For ESMF application use (communications)
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

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_VM.F90,v 1.15 2004/05/19 02:16:07 theurich Exp $'

!==============================================================================

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
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
!EOPI 
      end interface

!------------------------------------------------------------------------------
!BOPI
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
!EOPI 
      end interface

!------------------------------------------------------------------------------
!BOPI
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
!EOPI 
      end interface

!------------------------------------------------------------------------------
!BOPI
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
!EOPI 
      end interface


!==============================================================================
      

  contains
      
        
!==============================================================================
! ESMF_VM methods:
!==============================================================================


!------------------------------------------------------------------------------
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
  subroutine ESMF_VMGet(vm, localPet, petCount, peCount, mpiCommunicator, &
    okOpenMpFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    integer,            intent(out), optional :: localPet
    integer,            intent(out), optional :: petCount
    integer,            intent(out), optional :: peCount
    integer,            intent(out), optional :: mpiCommunicator
    type(ESMF_Logical), intent(out), optional :: okOpenMpFlag
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get VM internals
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[{[localPet]}]
!        Id of the PET that instantiates the local user code.
!   \item[{[petCount]}]
!        Number of PETs in VM.
!   \item[{[peCount]}]
!        Number of PEs referenced by VM.
!   \item[{[mpiCommunicator]}]
!        MPI Intracommunicator for VM.
!   \item[{[okOpenMpFlag]}]
!        Flag indicating whether user-level OpenMP threading is supported in VM.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGet(vm, localPet, petCount, peCount, mpiCommunicator, &
      okOpenMpFlag, rc)
    
  end subroutine ESMF_VMGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMGetPET - Get VM PET internals

! !INTERFACE:
  subroutine ESMF_VMGetPET(vm, pet, peCount, ssiId, threadCount, threadId, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
    integer,       intent(in)            :: pet
    integer,       intent(out), optional :: peCount
    integer,       intent(out), optional :: ssiId
    integer,       intent(out), optional :: threadCount
    integer,       intent(out), optional :: threadId
    integer,       intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get VM internals
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[pet] 
!        Id of the PET for which info is queried
!   \item[{[peCount]}]
!        Number of PEs associated with PET in VM.
!   \item[{[ssiId]}]
!        SSI id this PET is running on.
!   \item[{[threadCount]}]
!        Number of PETs in this PET's thread group.
!   \item[{[threadId]}]
!        Thread id of this PET.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGetPET(vm, pet, peCount, ssiId, threadCount, threadId, rc)
 
  end subroutine ESMF_VMGetPET
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_VMPrint - Print VM internals

! !INTERFACE:
  subroutine ESMF_VMPrint(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
    integer,       intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Print VM internals
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
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
! !IROUTINE: ESMF_VMSend - Send 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendI4(vm, message, count, dst, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),         intent(in)            :: vm
    integer(ESMF_KIND_I4), intent(in)            :: message(:)  
    integer,               intent(in)            :: count
    integer,               intent(in)            :: dst
    integer,               intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide send for ESMF\_KIND\_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[message] 
!        Array holding message data.
!   \item[count] 
!        Number of elements in message
!   \item[dst] 
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

    size = count * 4 ! 4 bytes
    call c_ESMC_VMSend(vm, message, size, dst, status)
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
! !IROUTINE: ESMF_VMSend - Send 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendR4(vm, message, count, dst, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R4), intent(in)            :: message(:)  
    integer,            intent(in)            :: count
    integer,            intent(in)            :: dst
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide send for ESMF\_KIND\_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[message] 
!        Array holding message data.
!   \item[count] 
!        Number of elements in message
!   \item[dst] 
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

    size = count * 4 ! 4 bytes
    call c_ESMC_VMSend(vm, message, size, dst, status)
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
! !IROUTINE: ESMF_VMSend - Send 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendR8(vm, message, count, dst, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R8), intent(in)            :: message(:)  
    integer,            intent(in)            :: count
    integer,            intent(in)            :: dst
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide send for ESMF\_KIND\_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[message] 
!        Array holding message data.
!   \item[count] 
!        Number of elements in message
!   \item[dst] 
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

    size = count * 8 ! 8 bytes
    call c_ESMC_VMSend(vm, message, size, dst, status)
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
! !IROUTINE: ESMF_VMRecv - Receive 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvI4(vm, message, count, source, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),         intent(in)            :: vm
    integer(ESMF_KIND_I4), intent(in)            :: message(:)  
    integer,               intent(in)            :: count
    integer,               intent(in)            :: source
    integer,               intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide receive for ESMF\_KIND\_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[message] 
!        Array holding message data
!   \item[count] 
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

    size = count * 4 ! 4 bytes
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
! !IROUTINE: ESMF_VMRecv - Receive 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvR4(vm, message, count, source, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R4), intent(in)            :: message(:)  
    integer,            intent(in)            :: count
    integer,            intent(in)            :: source
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide receive for ESMF\_KIND\_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[message] 
!        Array holding message data
!   \item[count] 
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

    size = count * 4 ! 4 bytes
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
! !IROUTINE: ESMF_VMRecv - Receive 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvR8(vm, message, count, source, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    real(ESMF_KIND_R8), intent(in)            :: message(:)  
    integer,            intent(in)            :: count
    integer,            intent(in)            :: source
    integer,            intent(out), optional :: rc           
!
! !DESCRIPTION:
!   VM wide receive for ESMF\_KIND\_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object
!   \item[message] 
!        Array holding message data
!   \item[count] 
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

    size = count * 4 ! 4 bytes
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
! !IROUTINE: ESMF_VMScatter - Scatter 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterI4(vm, input, output, count, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    integer(ESMF_KIND_I4), intent(in)      :: input(:)
    integer(ESMF_KIND_I4), intent(out)     :: output(:)
    integer, intent(in)                     :: count, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Scatter for ESMF\_KIND\_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holding input data
!   \item[output] 
!        Array holding output data
!   \item[count] 
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
    size = count * 4 ! 4 bytes
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
! !IROUTINE: ESMF_VMScatter - Scatter 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterR4(vm, input, output, count, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R4), intent(in)          :: input(:)
    real(ESMF_KIND_R4), intent(out)         :: output(:)
    integer, intent(in)                     :: count, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Scatter for ESMF\_KIND\_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holding input data
!   \item[output] 
!        Array holding output data
!   \item[count] 
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
    size = count * 4 ! 4 bytes
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
! !IROUTINE: ESMF_VMScatter - Scatter 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterR8(vm, input, output, count, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R8), intent(in)          :: input(:)
    real(ESMF_KIND_R8), intent(out)         :: output(:)
    integer, intent(in)                     :: count, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Scatter for ESMF\_KIND\_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holding input data
!   \item[output] 
!        Array holding output data
!   \item[count] 
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
    size = count * 8 ! 8 bytes
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
! !IROUTINE: ESMF_VMGather - Gather 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherI4(vm, input, output, count, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    integer(ESMF_KIND_I4), intent(in)       :: input(:)
    integer(ESMF_KIND_I4), intent(out)      :: output(:)
    integer, intent(in)                     :: count, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Gather for ESMF\_KIND\_I4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holding input data
!   \item[output] 
!        Array holding output data
!   \item[count] 
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
    size = count * 4 ! 4 bytes
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
! !IROUTINE: ESMF_VMGather - Gather 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherR4(vm, input, output, count, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R4), intent(in)          :: input(:)
    real(ESMF_KIND_R4), intent(out)         :: output(:)
    integer, intent(in)                     :: count, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Gather for ESMF\_KIND\_R4
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holding input data
!   \item[output] 
!        Array holding output data
!   \item[count] 
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
    size = count * 4 ! 4 bytes
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
! !IROUTINE: ESMF_VMGather - Gather 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherR8(vm, input, output, count, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)               :: vm
    real(ESMF_KIND_R8), intent(in)          :: input(:)
    real(ESMF_KIND_R8), intent(out)         :: output(:)
    integer, intent(in)                     :: count, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!   MPI-like VM wide Gather for ESMF\_KIND\_R8
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        VM object.
!   \item[input] 
!        Array holding input data
!   \item[output] 
!        Array holding output data
!   \item[count] 
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
    size = count * 8 ! 8 bytes
    call c_ESMC_VMGather(vm, input, output, size, root, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_VMGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMGatherR8
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
!BOPI
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
!EOPI
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
!BOPI
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
!EOPI
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

!==============================================================================
! ESMF_VMPlan methods:
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanConstruct(vmplan, vm, npetlist, petlist, rc)

  end subroutine ESMF_VMPlanConstruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanDestruct(vmplan, rc)

  end subroutine ESMF_VMPlanDestruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, rc)
  
  end subroutine ESMF_VMPlanMaxThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMinThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, rc)
  
  end subroutine ESMF_VMPlanMinThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxPEs(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, rc)
  
  end subroutine ESMF_VMPlanMaxPEs
!------------------------------------------------------------------------------


end module ESMF_VMMod
