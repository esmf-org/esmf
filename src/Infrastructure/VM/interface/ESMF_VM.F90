! $Id: ESMF_VM.F90,v 1.22 2004/05/21 20:28:19 theurich Exp $
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
  use ESMF_LogErrMod
      
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
  ! For ESMF application use
  public ESMF_VMGetGlobal
  public ESMF_VMGet
  public ESMF_VMGetPET
  public ESMF_VMPrint
  ! For ESMF application use (communications)
  public ESMF_VMSend
  public ESMF_VMRecv
  public ESMF_VMSendRecv
  public ESMF_VMScatter
  public ESMF_VMGather
  public ESMF_VMAllReduce
  public ESMF_VMAllGlobalReduce
  public ESMF_VMBarrier
  public ESMF_VMThreadBarrier
  public ESMF_VMWait
  ! For ESMF internal use only
  public ESMF_VMInitialize
  public ESMF_VMFinalize
  
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
      '$Id: ESMF_VM.F90,v 1.22 2004/05/21 20:28:19 theurich Exp $'

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
! !IROUTINE: ESMF_VMSendRecv -- Generic interface

! !INTERFACE:
      interface ESMF_VMSendRecv

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMSendRecvI4
      module procedure ESMF_VMSendRecvR4
      module procedure ESMF_VMSendRecvR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMSendRecv} functions.   
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
! !IROUTINE: ESMF_VMGather -- Generic interface

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

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_VMAllReduce -- Generic interface

! !INTERFACE:
      interface ESMF_VMAllReduce

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMAllReduceI4
      module procedure ESMF_VMAllReduceR4
      module procedure ESMF_VMAllReduceR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllReduce} functions.   
!EOPI 
      end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_VMAllGlobalReduce -- Generic interface

! !INTERFACE:
      interface ESMF_VMAllGlobalReduce

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMAllGlobalReduceI4
      module procedure ESMF_VMAllGlobalReduceR4
      module procedure ESMF_VMAllGlobalReduceR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllGlobalReduce} functions.   
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
    vm = GlobalVM

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMGetGlobal
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
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

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMGet
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGetPET()"
!BOP
! !IROUTINE: ESMF_VMGetPET - Get VM PET internals

! !INTERFACE:
  subroutine ESMF_VMGetPET(vm, pet, peCount, ssiId, threadCount, threadId, rc)
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
    call c_ESMC_VMGetPET(vm, pet, peCount, ssiId, threadCount, threadId, &
      localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMGetPET
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
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
!   Print internal information about the specified {\tt ESMF\_VM} object.
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

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMPrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendI4()"
!BOP
! !IROUTINE: ESMF_VMSend - Send 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendI4(vm, sendData, count, dst, blockingFlag, &
    commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4),    intent(in)              :: sendData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contigous data of kind {\tt ESMF\_KIND\_I4} to a PET within the same
!   {\tt ESMF\_VM} object. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contigous data array holding data to be send.
!   \item[count] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMSend(vm, sendData, size, dst, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMSendI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendR4()"
!BOP
! !IROUTINE: ESMF_VMSend - Send 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendR4(vm, sendData, count, dst, blockingFlag, &
    commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4),       intent(in)              :: sendData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contigous data of kind {\tt ESMF\_KIND\_R4} to a PET within the same
!   {\tt ESMF\_VM} object. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contigous data array holding data to be send.
!   \item[count] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMSend(vm, sendData, size, dst, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMSendR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendR8()"
!BOP
! !IROUTINE: ESMF_VMSend - Send 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendR8(vm, sendData, count, dst, blockingFlag, &
    commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8),       intent(in)              :: sendData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contigous data of kind {\tt ESMF\_KIND\_R8} to a PET within the same
!   {\tt ESMF\_VM} object. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contigous data array holding data to be send.
!   \item[count] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMSend(vm, sendData, size, dst, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMSendR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvI4()"
!BOP
! !IROUTINE: ESMF_VMRecv - Receive 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvI4(vm, recvData, count, src, blockingFlag, &
    commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4),    intent(in)              :: recvData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive contigous data of kind {\tt ESMF\_KIND\_I4} from a PET within the
!   same {\tt ESMF\_VM} object. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contigous data array for data to be received.
!   \item[count] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMRecv(vm, recvData, size, src, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMRecvI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvR4()"
!BOP
! !IROUTINE: ESMF_VMRecv - Receive 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvR4(vm, recvData, count, src, blockingFlag, &
    commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4),       intent(in)              :: recvData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive contigous data of kind {\tt ESMF\_KIND\_R4} from a PET within the
!   same {\tt ESMF\_VM} object. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contigous data array for data to be received.
!   \item[count] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMRecv(vm, recvData, size, src, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMRecvR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvR8()"
!BOP
! !IROUTINE: ESMF_VMRecv - Receive 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvR8(vm, recvData, count, src, blockingFlag, &
    commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8),       intent(in)              :: recvData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive contigous data of kind {\tt ESMF\_KIND\_R8} from a PET within the
!   same {\tt ESMF\_VM} object. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contigous data array for data to be received.
!   \item[count] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMRecv(vm, recvData, size, src, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMRecvR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvI4()"
!BOP
! !IROUTINE: ESMF_VMSendRecv - SendRecv 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvI4(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4),    intent(in)              :: sendData(:)  
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    integer(ESMF_KIND_I4),    intent(in)              :: recvData(:)  
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contigous data of kind {\tt ESMF\_KIND\_I4} to a PET within the same
!   {\tt ESMF\_VM} object while receiving contigous data of kind 
!   {\tt ESMF\_KIND\_I4} from a PET within the same {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contigous data array holding data to be send.
!   \item[sendCount] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contigous data array for data to be received.
!   \item[recvCount] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: sendSize, recvSize

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMSendRecv(vm, sendData, sendSize, dst, &
      recvData, recvSize, src, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMSendRecvI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvR4()"
!BOP
! !IROUTINE: ESMF_VMSendRecv - SendRecv 4-byte real

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvR4(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4),       intent(in)              :: sendData(:)  
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    real(ESMF_KIND_R4),       intent(in)              :: recvData(:)  
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contigous data of kind {\tt ESMF\_KIND\_R4} to a PET within the same
!   {\tt ESMF\_VM} object while receiving contigous data of kind 
!   {\tt ESMF\_KIND\_R4} from a PET within the same {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contigous data array holding data to be send.
!   \item[sendCount] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contigous data array for data to be received.
!   \item[recvCount] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: sendSize, recvSize

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMSendRecv(vm, sendData, sendSize, dst, &
      recvData, recvSize, src, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMSendRecvR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvR8()"
!BOP
! !IROUTINE: ESMF_VMSendRecv - SendRecv 8-byte real

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvR8(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8),       intent(in)              :: sendData(:)  
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    real(ESMF_KIND_R8),       intent(in)              :: recvData(:)  
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contigous data of kind {\tt ESMF\_KIND\_R8} to a PET within the same
!   {\tt ESMF\_VM} object while receiving contigous data of kind 
!   {\tt ESMF\_KIND\_R8} from a PET within the same {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contigous data array holding data to be send.
!   \item[sendCount] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contigous data array for data to be received.
!   \item[recvCount] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: sendSize, recvSize

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    sendSize = sendCount * 8 ! 8 bytes
    recvSize = recvCount * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMSendRecv(vm, sendData, sendSize, dst, &
      recvData, recvSize, src, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMSendRecvR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterI4()"
!BOP
! !IROUTINE: ESMF_VMScatter - Scatter 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterI4(vm, sendData, recvData, count, root, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4),    intent(in)              :: sendData(:)
    integer(ESMF_KIND_I4),    intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that scatters contigous data 
!   of kind {\tt ESMF\_KIND\_I4} across the PETs of an {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. Only the {\tt sendData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements to be send from {\tt root} to each of the PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMScatter(vm, sendData, recvData, size, root, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMScatterI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterR4()"
!BOP
! !IROUTINE: ESMF_VMScatter - Scatter 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterR4(vm, sendData, recvData, count, root, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R4),       intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that scatters contigous data 
!   of kind {\tt ESMF\_KIND\_R4} across the PETs of an {\tt ESMF\_VM} object.
!
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. Only the {\tt sendData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements to be send from {\tt root} to each of the PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMScatter(vm, sendData, recvData, size, root, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMScatterR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterR8()"
!BOP
! !IROUTINE: ESMF_VMScatter - Scatter 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterR8(vm, sendData, recvData, count, root, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R8),       intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that scatters contigous data 
!   of kind {\tt ESMF\_KIND\_R8} across the PETs of an {\tt ESMF\_VM} object.
!
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. Only the {\tt sendData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements to be send from {\tt root} to each of the PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMScatter(vm, sendData, recvData, size, root, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMScatterR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherI4()"
!BOP
! !IROUTINE: ESMF_VMGather - Gather 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherI4(vm, sendData, recvData, count, root, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4),    intent(in)              :: sendData(:)
    integer(ESMF_KIND_I4),    intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contigous data 
!   of kind {\tt ESMF\_KIND\_I4} from the PETs of an {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. Only the {\tt recvData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[count] 
!        Number of elements to be send from {\tt root} to each of the PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGather(vm, sendData, recvData, size, root, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMGatherI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherR4()"
!BOP
! !IROUTINE: ESMF_VMGather - Gather 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherR4(vm, sendData, recvData, count, root, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R4),       intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contigous data 
!   of kind {\tt ESMF\_KIND\_R4} from the PETs of an {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. Only the {\tt recvData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[count] 
!        Number of elements to be send from {\tt root} to each of the PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGather(vm, sendData, recvData, size, root, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMGatherR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherR8()"
!BOP
! !IROUTINE: ESMF_VMGather - Gather 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherR8(vm, sendData, recvData, count, root, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R8),       intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contigous data 
!   of kind {\tt ESMF\_KIND\_R8} from the PETs of an {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. Only the {\tt recvData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[count] 
!        Number of elements to be send from {\tt root} to each of the PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGather(vm, sendData, recvData, size, root, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMGatherR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceI4()"
!BOP
! !IROUTINE: ESMF_VMAllReduce - AllReduce 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceI4(vm, sendData, recvData, count, operation, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4),    intent(in)              :: sendData(:)
    integer(ESMF_KIND_I4),    intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_Operation),     intent(in)              :: operation
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs an AllReduce 
!   on contigous data of kind {\tt ESMF\_KIND\_I4} across the {\tt ESMF\_VM}
!   object performing the specified operation.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements in sendData on each of the PETs.
!   \item[operation] 
!        Reduction operation.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllReduce(vm, sendData, recvData, count, ESMF_I4, operation, &
      localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMAllReduceI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceR4()"
!BOP
! !IROUTINE: ESMF_VMAllReduce - AllReduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceR4(vm, sendData, recvData, count, operation, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R4),       intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_Operation),     intent(in)              :: operation
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs an AllReduce 
!   on contigous data of kind {\tt ESMF\_KIND\_R4} across the {\tt ESMF\_VM}
!   object performing the specified operation.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements in sendData on each of the PETs.
!   \item[operation] 
!        Reduction operation.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllReduce(vm, sendData, recvData, count, ESMF_R4, operation, &
      localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMAllReduceR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceR8()"
!BOP
! !IROUTINE: ESMF_VMAllReduce - AllReduce 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceR8(vm, sendData, recvData, count, operation, &
    blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R8),       intent(out)             :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_Operation),     intent(in)              :: operation
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs an AllReduce 
!   on contigous data of kind {\tt ESMF\_KIND\_R8} across the {\tt ESMF\_VM}
!   object performing the specified operation.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements in sendData on each of the PETs.
!   \item[operation] 
!        Reduction operation.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllReduce(vm, sendData, recvData, count, ESMF_R8, operation, &
      localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMAllReduceR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGlobalReduceI4()"
!BOP
! !IROUTINE: ESMF_VMAllGlobalReduce - AllGlobalReduce 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGlobalReduce()
  subroutine ESMF_VMAllGlobalReduceI4(vm, sendData, recvData, count, &
    operation,  blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4),    intent(in)              :: sendData(:)
    integer(ESMF_KIND_I4),    intent(out)             :: recvData
    integer,                  intent(in)              :: count
    type(ESMF_Operation),     intent(in)              :: operation
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs an
!   AllGlobalReduce on contigous data of kind {\tt ESMF\_KIND\_I4} across the
!   {\tt ESMF\_VM} object performing the specified operation.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements in sendData on each of the PETs.
!   \item[operation] 
!        Reduction operation.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllGlobalReduce(vm, sendData, recvData, count, ESMF_I4, &
      operation, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMAllGlobalReduceI4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGlobalReduceR4()"
!BOP
! !IROUTINE: ESMF_VMAllGlobalReduce - AllGlobalReduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGlobalReduce()
  subroutine ESMF_VMAllGlobalReduceR4(vm, sendData, recvData, count, &
    operation, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R4),       intent(out)             :: recvData
    integer,                  intent(in)              :: count
    type(ESMF_Operation),     intent(in)              :: operation
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs an
!   AllGlobalReduce on contigous data of kind {\tt ESMF\_KIND\_R4} across the
!   {\tt ESMF\_VM} object performing the specified operation.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements in sendData on each of the PETs.
!   \item[operation] 
!        Reduction operation.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllGlobalReduce(vm, sendData, recvData, count, ESMF_R4, &
      operation, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMAllGlobalReduceR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGlobalReduceR8()"
!BOP
! !IROUTINE: ESMF_VMAllGlobalReduce - AllGlobalReduce 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGlobalReduce()
  subroutine ESMF_VMAllGlobalReduceR8(vm, sendData, recvData, count, &
    operation, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8),       intent(in)              :: sendData(:)
    real(ESMF_KIND_R8),       intent(out)             :: recvData
    integer,                  intent(in)              :: count
    type(ESMF_Operation),     intent(in)              :: operation
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs an
!   AllGlobalReduce on contigous data of kind {\tt ESMF\_KIND\_R4} across the
!   {\tt ESMF\_VM} object performing the specified operation.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contigous data arry holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contigous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements in sendData on each of the PETs.
!   \item[operation] 
!        Reduction operation.
!   \item[{[blockingFlag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             Block until local operation has completed. 
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commHandle]}]
!        A communication handle will be returned in case of a non-blocking
!        request (see argument {\tt blockingFlag}).
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
    integer :: size

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Flag not implemented features
    if (present(blockingFlag)) then
      if (blockingFlag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite('Non-blocking not implemented.', ESMF_LOG_ERROR)
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllGlobalReduce(vm, sendData, recvData, count, ESMF_R8, &
      operation, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMAllGlobalReduceR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBarrier()"
!BOP
! !IROUTINE: ESMF_VMBarrier - VM wide barrier

! !INTERFACE:
  subroutine ESMF_VMBarrier(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)              :: vm
    integer,        intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that blocks calling PET until
!   all of the PETs have issued the call. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
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
    call c_ESMC_VMBarrier(vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMBarrier
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMThreadBarrier()"
!BOP
! !IROUTINE: ESMF_VMThreadBarrier - PET thread group wide barrier

! !INTERFACE:
  subroutine ESMF_VMThreadBarrier(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)              :: vm
    integer,        intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Partially collective {\tt ESMF\_VM} communication call that blocks calling
!   PET until all of the PETs that are running under the same POSIX process have
!   issued the call. 
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
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
    call c_ESMC_VMThreadBarrier(vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMThreadBarrier
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMWait()"
!BOP
! !IROUTINE: ESMF_VMWait - Wait for non-blocking VM communication to complete

! !INTERFACE:
  subroutine ESMF_VMWait(vm, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),          intent(in)              :: vm
    type(ESMF_CommHandle),  intent(in)              :: commHandle
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Wait for non-blocking VM communication to complete.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[commHandle] 
!        Handle specifying a previous non-blocking communication request.
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
    ! TODO: call c_ESMC_VMWait(vm, ...)
    localrc = ESMF_FAILURE  ! until there is really an implementation

    ! Use LogErr to handle return code
!    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
!      rcToReturn=rc)) return
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg="Method not implemented", &
      rcToReturn=rc)) return

  end subroutine ESMF_VMWait
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMInitialize()"
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
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMInitialize(GlobalVM, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMInitialize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMFinalize()"
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
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMFinalize(localrc)
    
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMFinalize
!------------------------------------------------------------------------------


!==============================================================================
! ESMF_VMPlan methods:
!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanConstruct()"
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
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanConstruct(vmplan, vm, npetlist, petlist, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMPlanConstruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanDestruct()"
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
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanDestruct(vmplan, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMPlanDestruct
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanMaxThreads()"
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
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMPlanMaxThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanMinThreads()"
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
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMinThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMPlanMinThreads
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanMaxPEs()"
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
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxPEs(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(rcToCheck=localrc, msg=ESMF_ERR_PASSTHRU, &
      rcToReturn=rc)) return

  end subroutine ESMF_VMPlanMaxPEs
!------------------------------------------------------------------------------


end module ESMF_VMMod
