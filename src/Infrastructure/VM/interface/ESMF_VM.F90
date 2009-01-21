! $Id: ESMF_VM.F90,v 1.97.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_VM.F90"
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
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
      
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_CommHandle
!      
  type ESMF_CommHandle
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type
      
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_VM
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

  ! F90 class type to hold pointer to C++ object
  type ESMF_VMPlan
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

  ! F90 class type to hold pointer to C++ object
  type ESMF_VMId
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
  public ESMF_VMId
      
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

  type(ESMF_VM), save :: GlobalVM     ! This is a reference to the global VM
  public GlobalVM
  
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_VMAllFullReduce
  public ESMF_VMAllGather
  public ESMF_VMAllGatherV
  public ESMF_VMAllReduce
  public ESMF_VMAllToAllV
  public ESMF_VMBarrier
  public ESMF_VMBroadcast
  public ESMF_VMCommWait
  public ESMF_VMCommQueueWait
  public ESMF_VMGather
  public ESMF_VMGatherV
  public ESMF_VMGet
  public ESMF_VMGetGlobal
  public ESMF_VMGetCurrent
  public ESMF_VMGetCurrentID
  public ESMF_VMGetPETLocalInfo
  public ESMF_VMGetVMId
  public ESMF_VMPrint
  public ESMF_VMRecv
  public ESMF_VMRecvVMId
  public ESMF_VMReduce
  public ESMF_VMScatter
  public ESMF_VMScatterV
  public ESMF_VMSend
  public ESMF_VMSendVMId
  public ESMF_VMSendRecv
  public ESMF_VMThreadBarrier
  public ESMF_VMValidate
  public ESMF_VMWtime
  public ESMF_VMWtimeDelay
  public ESMF_VMWtimePrec
  
  public ESMF_CommHandleValidate
  
! - ESMF-internal methods:
  public ESMF_VMInitialize
  public ESMF_VMFinalize
  public ESMF_VMAbort
  public ESMF_VMShutdown
  public ESMF_VMGetInit
  public ESMF_VMSetInitCreated
  public ESMF_VMGetThis
  public ESMF_VMSetThis
  public ESMF_VMPlanConstruct
  public ESMF_VMPlanDestruct
  public ESMF_VMPlanGetInit
  public ESMF_VMPlanGetThis
  public ESMF_VMPlanSetThis
  public ESMF_VMPlanMaxPEs
  public ESMF_VMPlanMaxThreads
  public ESMF_VMPlanMinThreads
  public ESMF_VMIdCompare
  public ESMF_VMIdPrint
  public ESMF_VMIdCreate
  public ESMF_VMIdDestroy

  public ESMF_CommHandleGetInit

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      "$Id: ESMF_VM.F90,v 1.97.2.4 2009/01/21 21:25:24 cdeluca Exp $"

!==============================================================================

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMAllFullReduce -- Generic interface

! !INTERFACE:
      interface ESMF_VMAllFullReduce

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMAllFullReduceI4
      module procedure ESMF_VMAllFullReduceR4
      module procedure ESMF_VMAllFullReduceR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllFullReduce} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMAllGather -- Generic interface

! !INTERFACE:
      interface ESMF_VMAllGather

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMAllGatherI4
      module procedure ESMF_VMAllGatherR4
      module procedure ESMF_VMAllGatherR8
      module procedure ESMF_VMAllGatherLogical

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllGather} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMAllGatherV -- Generic interface

! !INTERFACE:
      interface ESMF_VMAllGatherV

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMAllGatherVI4
      module procedure ESMF_VMAllGatherVR4
      module procedure ESMF_VMAllGatherVR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllGatherV} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
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

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMAllToAllV -- Generic interface

! !INTERFACE:
      interface ESMF_VMAllToAllV

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMAllToAllVI4
      module procedure ESMF_VMAllToAllVR4
      module procedure ESMF_VMAllToAllVR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllToAllV} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMBroadcast -- Generic interface

! !INTERFACE:
      interface ESMF_VMBroadcast

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMBroadcastI4
      module procedure ESMF_VMBroadcastR4
      module procedure ESMF_VMBroadcastR8
      module procedure ESMF_VMBroadcastLogical

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMBroadcast} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMGather -- Generic interface

! !INTERFACE:
      interface ESMF_VMGather

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMGatherI4
      module procedure ESMF_VMGatherR4
      module procedure ESMF_VMGatherR8
      module procedure ESMF_VMGatherLogical

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMGather} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMGatherV -- Generic interface

! !INTERFACE:
      interface ESMF_VMGatherV

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMGatherVI4
      module procedure ESMF_VMGatherVR4
      module procedure ESMF_VMGatherVR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMGatherV} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMRecv -- Generic interface

! !INTERFACE:
      interface ESMF_VMRecv

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMRecvI4
      module procedure ESMF_VMRecvR4
      module procedure ESMF_VMRecvR8
      module procedure ESMF_VMRecvLogical
      module procedure ESMF_VMRecvCharacter

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMRecv} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMReduce -- Generic interface

! !INTERFACE:
      interface ESMF_VMReduce

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMReduceI4
      module procedure ESMF_VMReduceR4
      module procedure ESMF_VMReduceR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMReduce} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMScatter -- Generic interface

! !INTERFACE:
      interface ESMF_VMScatter

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMScatterI4
      module procedure ESMF_VMScatterR4
      module procedure ESMF_VMScatterR8
      module procedure ESMF_VMScatterLogical

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMScatter} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMScatterV -- Generic interface

! !INTERFACE:
      interface ESMF_VMScatterV

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMScatterVI4
      module procedure ESMF_VMScatterVR4
      module procedure ESMF_VMScatterVR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMScatterV} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMSend -- Generic interface

! !INTERFACE:
      interface ESMF_VMSend

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMSendI4
      module procedure ESMF_VMSendR4
      module procedure ESMF_VMSendR8
      module procedure ESMF_VMSendLogical
      module procedure ESMF_VMSendCharacter

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMSend} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMSendRecv -- Generic interface

! !INTERFACE:
      interface ESMF_VMSendRecv

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMSendRecvI4
      module procedure ESMF_VMSendRecvR4
      module procedure ESMF_VMSendRecvR8
      module procedure ESMF_VMSendRecvLogical
      module procedure ESMF_VMSendRecvCharacter

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMSendRecv} functions.   
!EOPI 
      end interface


!==============================================================================
      

  contains
      
        
! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllFullReduce - Fully reduce data across VM, result on all PETs
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMAllFullReduce()
!  subroutine ESMF_VMAllFullReduce<type><kind>(vm, sendData, recvData, count, &
!    reduceflag,  blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,    intent(in)   :: sendData(:)
!    <type>(ESMF_KIND_<kind>), intent(out)             :: recvData
!    integer,                  intent(in)              :: count
!    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that reduces a contiguous data 
!   array of <type><kind> across the {\tt ESMF\_VM} object 
!   into a single value of the same <type><kind>. The result is
!   returned on all PETs. Different reduction operations can be specified.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!   \newline
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt blockingflag = ESMF\_NONBLOCKING} error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[count] 
!        Number of elements in sendData. Must be the same on all PETs.
!   \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllFullReduceI4()"
!BOPI
! !IROUTINE: ESMF_VMAllFullReduce - AllFullReduce 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllFullReduce()
  subroutine ESMF_VMAllFullReduceI4(vm, sendData, recvData, count, &
    reduceflag,  blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,    intent(in)      :: sendData(:)
    integer(ESMF_KIND_I4),    intent(out)             :: recvData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllFullReduce(vm, sendData(1), recvData, count, ESMF_TYPEKIND_I4, &
      reduceflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllFullReduceI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllFullReduceR4()"
!BOPI
! !IROUTINE: ESMF_VMAllFullReduce - AllFullReduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllFullReduce()
  subroutine ESMF_VMAllFullReduceR4(vm, sendData, recvData, count, &
    reduceflag, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R4),       intent(out)             :: recvData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllFullReduce(vm, sendData(1), recvData, count, ESMF_TYPEKIND_R4, &
      reduceflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllFullReduceR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllFullReduceR8()"
!BOPI
! !IROUTINE: ESMF_VMAllFullReduce - AllFullReduce 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllFullReduce()
  subroutine ESMF_VMAllFullReduceR8(vm, sendData, recvData, count, &
    reduceflag, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R8),       intent(out)             :: recvData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllFullReduce(vm, sendData(1), recvData, count, ESMF_TYPEKIND_R8, &
      reduceflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllFullReduceR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllGather - Gather data across VM, result on all PETs
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMAllGather()
!  subroutine ESMF_VMAllGather<type><kind>(vm, sendData, recvData, count, &
!    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: count
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object into an array on all PETs.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid {\tt recvData} argument.
!   \item[count] 
!        Number of elements to be gathered from each PET. Must be the
!        same on all PETs.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherI4()"
!BOPI
! !IROUTINE: ESMF_VMAllGather - AllGather 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGather()
  subroutine ESMF_VMAllGatherI4(vm, sendData, recvData, count, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData(1), recvData(1), size, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData(1), recvData(1), size, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherR4()"
!BOPI
! !IROUTINE: ESMF_VMAllGather - AllGather 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGather()
  subroutine ESMF_VMAllGatherR4(vm, sendData, recvData, count, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R4), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData, recvData, size, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData, recvData, size, localcommhandle, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherR8()"
!BOPI
! !IROUTINE: ESMF_VMAllGather - AllGather 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGather()
  subroutine ESMF_VMAllGatherR8(vm, sendData, recvData, count, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R8), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData(1), recvData(1), size, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData(1), recvData(1), size, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherLogical()"
!BOPI
! !IROUTINE: ESMF_VMAllGather - AllGather ESMF_Logical

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGather()
  subroutine ESMF_VMAllGatherLogical(vm, sendData, recvData, count, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_Logical), target,      intent(in)       :: sendData(:)
    type(ESMF_Logical), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData(1), recvData(1), size, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData(1), recvData(1), size, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllGatherV - GatherV data across VM, result on all PETs
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMAllGatherV()
!  subroutine ESMF_VMAllGatherV<type><kind>(vm, sendData, sendCount, recvData, &
!    recvCounts, recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    integer,                  intent(in)              :: sendCount
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: recvCounts(:)
!    integer,                  intent(in)              :: recvOffsets(:)
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object into an array on all PETs.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}. 
!   \newline
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt blockingflag = ESMF\_NONBLOCKING} error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[sendCount] 
!        Number of {\tt sendData} elements to send from local PET to all other
!        PETs.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[recvCounts] 
!        Number of {\tt recvData} elements to be received from corresponding
!        source PET.
!   \item[recvOffsets] 
!        Offsets in units of elements in {\tt recvData} marking the start of
!        element sequence to be received from source PET.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherVI4()"
!BOPI
! !IROUTINE: ESMF_VMAllGatherV - AllGatherV 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGatherV()
  subroutine ESMF_VMAllGatherVI4(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer,                  intent(in)              :: sendCount
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllGatherV(vm, sendData(1), sendCount, &
      recvData(1), recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I4, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherVR4()"
!BOPI
! !IROUTINE: ESMF_VMAllGatherV - AllGatherV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGatherV()
  subroutine ESMF_VMAllGatherVR4(vm, sendData, sendCount, recvData, &
    recvCounts,  recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCount
    real(ESMF_KIND_R4), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllGatherV(vm, sendData(1), sendCount, &
      recvData(1), recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R4, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherVR8()"
!BOPI
! !IROUTINE: ESMF_VMAllGatherV - AllGatherV 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGatherV()
  subroutine ESMF_VMAllGatherVR8(vm, sendData, sendCount, recvData, &
    recvCounts,  recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCount
    real(ESMF_KIND_R8), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllGatherV(vm, sendData(1), sendCount, &
      recvData(1), recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R8, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllReduce - Reduce data across VM, result on all PETs
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMAllReduce()
!  subroutine ESMF_VMAllReduce<type><kind>(vm, sendData, recvData, count, &
!    reduceflag, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: count
!    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!         
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that reduces a contiguous data 
!   array across the {\tt ESMF\_VM} object into a contiguous data array of the
!   same <type><kind>. The result array is returned on all PETs. 
!   Different reduction operations can be specified.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}. 
!   \newline
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt blockingflag = ESMF\_NONBLOCKING} error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[count] 
!        Number of elements in sendData and recvData. Must be the same on all
!        PETs.
!   \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceI4()"
!BOPI
! !IROUTINE: ESMF_VMAllReduce - AllReduce 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceI4(vm, sendData, recvData, count, reduceflag, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllReduce(vm, sendData(1), recvData(1), count, ESMF_TYPEKIND_I4, &
      reduceflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllReduceI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceR4()"
!BOPI
! !IROUTINE: ESMF_VMAllReduce - AllReduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceR4(vm, sendData, recvData, count, reduceflag, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R4), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllReduce(vm, sendData(1), recvData(1), count, ESMF_TYPEKIND_R4, &
      reduceflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllReduceR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceR8()"
!BOPI
! !IROUTINE: ESMF_VMAllReduce - AllReduce 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceR8(vm, sendData, recvData, count, reduceflag, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R8), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllReduce(vm, sendData(1), recvData(1), count, ESMF_TYPEKIND_R8, &
      reduceflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllReduceR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV communications across VM
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMAllToAllV()
!  subroutine ESMF_VMAllToAllV<type><kind>(vm, sendData, sendCounts, sendOffsets, &
!    recvData, recvCounts, recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    integer,                  intent(in)              :: sendCounts(:)
!    integer,                  intent(in)              :: sendOffsets(:)
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: recvCounts(:)
!    integer,                  intent(in)              :: recvOffsets(:)
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs a total exchange
!   operation, sending pieces of the contiguous data buffer {\tt semdData} to
!   all other PETs while receiving data into the contiguous data buffer
!   {\tt recvData} from all other PETs.\newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}. 
!   \newline
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt blockingflag = ESMF\_NONBLOCKING} error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[sendCounts] 
!        Number of {\tt sendData} elements to send from local PET to
!        destination PET.
!   \item[sendOffsets] 
!        Offsets in units of elements in {\tt sendData} marking to start of
!        element sequence to be send from local PET to destination PET.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[recvCounts] 
!        Number of {\tt recvData} elements to be received by local PET from
!        source PET.
!   \item[recvOffsets] 
!        Offsets in units of elements in {\tt recvData} marking to start of
!        element sequence to be received by local PET from source PET.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllVI4()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVI4(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCounts, recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer,                  intent(in)              :: sendCounts(:)
    integer,                  intent(in)              :: sendOffsets(:)
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllToAllV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I4, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllVR4()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVR4(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCounts, recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCounts(:)
    integer,                  intent(in)              :: sendOffsets(:)
    real(ESMF_KIND_R4), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllToAllV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R4, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllVR8()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVR8(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCounts, recvOffsets, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCounts(:)
    integer,                  intent(in)              :: sendOffsets(:)
    real(ESMF_KIND_R8), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAllToAllV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R8, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
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
!   all PETs of the VM context have issued the call.\newline
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMBarrier(vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBarrier
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMBroadcast - Broadcast data across VM
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMBroadcast()
!  subroutine ESMF_VMBroadcast<type><kind>(vm, bcstData, count, root, &
!    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(inout) :: bcstData(:)
!    integer,                  intent(in)              :: count
!    integer,                  intent(in)              :: root
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that broadcasts a contiguous 
!   data array from PET {\tt root} to all other PETs of the {\tt ESMF\_VM}
!   object.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[bcstData]
!        Contiguous data array. On {\tt root} PET {\tt bcstData} holds data that
!        is to be broadcasted to all other PETs. On all other PETs 
!        {\tt bcstData} is used to receive the broadcasted data.
!   \item[count] 
!        Number of elements in sendData and recvData. Must be the same on all
!        PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastI4()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastI4(vm, bcstData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(inout)    :: bcstData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMBroadcast(vm, bcstData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, root, localcommhandle, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastR4()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastR4(vm, bcstData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(inout)    :: bcstData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMBroadcast(vm, bcstData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, root, localcommhandle, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastR8()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastR8(vm, bcstData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(inout)    :: bcstData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMBroadcast(vm, bcstData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, root, localcommhandle, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastLogical()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast ESMF_Logical

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastLogical(vm, bcstData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_Logical), target,      intent(inout)    :: bcstData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMBroadcast(vm, bcstData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, root, localcommhandle, &
        localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMGather - Gather data from across VM
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMGather()
!  subroutine ESMF_VMGather<type><kind>(vm, sendData, recvData, count, root, &
!    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: count
!    integer,                  intent(in)              :: root
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object (including {\tt root}) into an
!   array on the {\tt root} PET.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contiguous data array for data to be received. Only the {\tt recvData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[count] 
!        Number of elements to be send from each PET to {\tt root}. Must be the
!        same on all PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherI4()"
!BOPI
! !IROUTINE: ESMF_VMGather - Gather 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherI4(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherR4()"
!BOPI
! !IROUTINE: ESMF_VMGather - Gather 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherR4(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R4), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherR8()"
!BOPI
! !IROUTINE: ESMF_VMGather - Gather 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherR8(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R8), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherLogical()"
!BOPI
! !IROUTINE: ESMF_VMGather - Gather ESMF_Logical

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherLogical(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_Logical), target,      intent(in)       :: sendData(:)
    type(ESMF_Logical), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMGatherV - GatherV data from across VM
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMGatherV()
!  subroutine ESMF_VMGatherV<type><kind>(vm, sendData, sendCount, recvData, &
!    recvCounts, recvOffsets, root, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    integer,                  intent(in)              :: sendCount
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: recvCounts(:)
!    integer,                  intent(in)              :: recvOffsets(:)
!    integer,                  intent(in)              :: root
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object into an array on root PET.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!   \newline
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt blockingflag = ESMF\_NONBLOCKING} error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[sendCount] 
!        Number of {\tt sendData} elements to send from local PET to all other
!        PETs.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[recvCounts] 
!        Number of {\tt recvData} elements to be received from corresponding
!        source PET.
!   \item[recvOffsets] 
!        Offsets in units of elements in {\tt recvData} marking the start of
!        element sequence to be received from source PET.
!   \item[root]
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherVI4()"
!BOPI
! !IROUTINE: ESMF_VMGatherV - GatherV 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMGatherV()
  subroutine ESMF_VMGatherVI4(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer,                  intent(in)              :: sendCount
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    integer,                  intent(in)              :: root
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGatherV(vm, sendData(1), sendCount, recvData(1), &
      recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I4, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherVR4()"
!BOPI
! !IROUTINE: ESMF_VMGatherV - GatherV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGatherV()
  subroutine ESMF_VMGatherVR4(vm, sendData, sendCount, recvData, &
    recvCounts,  recvOffsets, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCount
    real(ESMF_KIND_R4), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    integer,                  intent(in)              :: root
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGatherV(vm, sendData(1), sendCount, recvData(1), &
      recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R4, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherVR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherVR8()"
!BOPI
! !IROUTINE: ESMF_VMGatherV - AllGatherV 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGatherV()
  subroutine ESMF_VMGatherVR8(vm, sendData, sendCount, recvData, &
    recvCounts,  recvOffsets, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCount
    real(ESMF_KIND_R8), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCounts(:)
    integer,                  intent(in)              :: recvOffsets(:)
    integer,                  intent(in)              :: root
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGatherV(vm, sendData(1), sendCount, recvData(1), &
      recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R8, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGet()"
!BOP
! !IROUTINE: ESMF_VMGet - Get VM internals

! !INTERFACE:
  subroutine ESMF_VMGet(vm, localPet, petCount, peCount, mpiCommunicator, &
    supportPthreadsFlag, supportOpenMPFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)              :: vm
    integer,            intent(out),  optional  :: localPet
    integer,            intent(out),  optional  :: petCount
    integer,            intent(out),  optional  :: peCount
    integer,            intent(out),  optional  :: mpiCommunicator
    type(ESMF_Logical), intent(out),  optional  :: supportPthreadsFlag
    type(ESMF_Logical), intent(out),  optional  :: supportOpenMPFlag
    integer,            intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Get internal information about the specified {\tt ESMF\_VM} object.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        Queried {\tt ESMF\_VM} object.
!   \item[{[localPet]}]
!        Upon return this holds the id of the PET that issued this call.
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
!   \item[{[supportPthreadsFlag]}]
!        Upon return this holds a flag indicating whether Pthreads are
!        supported by the specified {\tt ESMF\_VM} object.
!        \begin{description}
!        \item[{\tt ESMF\_TRUE}]
!             Pthreads are supported.
!        \item[{\tt ESMF\_FALSE}]
!             Pthreads are not supported.
!        \end{description}
!   \item[{[supportOpenMPFlag]}]
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGet(vm, localPet, petCount, peCount, mpiCommunicator, &
      supportPthreadsFlag, supportOpenMPFlag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

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
!   object that is created during {\tt ESMF\_Initialize()} and is the ultimate
!   parent of all {\tt ESMF\_VM} objects in an ESMF application.\newline
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
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Copy the handle to the global VM into the output variable
    vm = GlobalVM

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetGlobal
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMGetCurrent - Get Current VM

! !INTERFACE:
  subroutine ESMF_VMGetCurrent(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(out)            :: vm
    integer,       intent(out), optional  :: rc           
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_VM} object of the current execution context.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!     Upon return this holds the {\tt ESMF\_VM} object of the current context.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGetCurrent(vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Set init code
    ESMF_INIT_SET_CREATED(vm)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetCurrent
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_VMGetCurrentID - Get Current VMId

! !INTERFACE:
  subroutine ESMF_VMGetCurrentID(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId), intent(out)            :: vmId
    integer,         intent(out), optional  :: rc           
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_VMId} of the current execution context.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vmId] 
!     Upon return this holds the {\tt ESMF\_VMId} of the current context.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGetCurrentID(vmId, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetCurrentID
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_VMGetVMId - Get VMId

! !INTERFACE:
  subroutine ESMF_VMGetVMId(vm, vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),   intent(in)             :: vm
    type(ESMF_VMId), intent(out)            :: vmId
    integer,         intent(out), optional  :: rc           
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_VMId} of the {\tt ESMF\_VM} object.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!     Queried {\tt ESMF\_VM} object.
!   \item[vmId] 
!     Upon return this holds the {\tt ESMF\_VMId} of the current context.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGetVMId(vm, vmId, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetVMId
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGetPETLocalInfo()"
!BOP
! !IROUTINE: ESMF_VMGetPETLocalInfo - Get VM PET local internals

! !INTERFACE:
  subroutine ESMF_VMGetPETLocalInfo(vm, pet, peCount, ssiId, threadCount, &
    threadId, vas, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)              :: vm
    integer,        intent(in)              :: pet
    integer,        intent(out),  optional  :: peCount
    integer,        intent(out),  optional  :: ssiId
    integer,        intent(out),  optional  :: threadCount
    integer,        intent(out),  optional  :: threadId
    integer,        intent(out),  optional  :: vas
    integer,        intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Get internal information about a specific PET within an {\tt ESMF\_VM} 
!   object.\newline
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
!        Upon return this holds the number of PETs in the specified PET"s 
!        thread group.
!   \item[{[threadId]}]
!        Upon return this holds the thread id of the specified PET within the 
!        PET"s thread group.
!   \item[{[vas]}]
!        Virtual address space in which this PET operates.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMGetPETLocalInfo(vm, pet, peCount, ssiId, threadCount, &
      threadId, vas, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

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
!   Print internal information about the specified {\tt ESMF\_VM} to
!   {\tt stdout}.\newline
!
!   Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!   On some platforms/compilers there is a potential issue with interleaving
!   Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!   the expected order.  If this occurs, it is recommended to use the
!   standard Fortran call {\tt flush(6)} as a workaround until this issue
!   is fixed in a future release. 
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPrint(vm, localrc) 
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMRecv - Receive data from src PET
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMRecv()
!  subroutine ESMF_VMRecv<type><kind>(vm, recvData, count, src, blockingflag, &
!    commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)  
!    integer,                  intent(in)              :: count
!    integer,                  intent(in)              :: src
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive contiguous data from {\tt src} PET within the same {\tt ESMF\_VM} 
!   object.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8},
!   {\tt ESMF\_TYPEKIND\_LOGICAL}, {\tt ESMF\_TYPEKIND\_CHARACTER}.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contiguous data array for data to be received.
!   \item[count] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvI4()"
!BOP!
! !IROUTINE: ESMF_VMRecv - Receive 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvI4(vm, recvData, count, src, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMRecv(vm, recvData(1), size, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMRecvNB(vm, recvData(1), size, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvR4()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvR4(vm, recvData, count, src, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMRecv(vm, recvData(1), size, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMRecvNB(vm, recvData(1), size, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvR8()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvR8(vm, recvData, count, src, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMRecv(vm, recvData(1), size, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMRecvNB(vm, recvData(1), size, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvLogical()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive ESMF_Logical

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvLogical(vm, recvData, count, src, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_Logical), target,      intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMRecv(vm, recvData(1), size, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMRecvNB(vm, recvData(1), size, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvCharacter()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive Character

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvCharacter(vm, recvData, count, src, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    character(*),             intent(out)             :: recvData
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 1 ! 1 byte
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMRecv(vm, recvData, size, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMRecvNB(vm, recvData, size, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvCharacter
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMReduce - Reduce data from across VM
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMReduce()
!  subroutine ESMF_VMReduce<type><kind>(vm, sendData, recvData, count, &
!    reduceflag, root, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: count
!    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
!    integer,                  intent(in)              :: root
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that reduces a contiguous data 
!   array across the {\tt ESMF\_VM} object into a contiguous data array of 
!   the same <type><kind>. The result array is returned  on root PET. 
!   Different reduction operations can be specified.
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!   \newline
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt blockingflag = ESMF\_NONBLOCKING} error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[count] 
!        Number of elements in sendData and recvData. Must be the same on all
!        PETs.
!   \item[reduceflag] 
!        Reduction operation. See section \ref{opt:reduceflag} for a list of 
!        valid reduce operations.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMReduceI4()"
!BOPI
! !IROUTINE: ESMF_VMReduce - Reduce 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMReduce()
  subroutine ESMF_VMReduceI4(vm, sendData, recvData, count, reduceflag, &
    root, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMReduce(vm, sendData(1), recvData(1), count, ESMF_TYPEKIND_I4, &
      reduceflag, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMReduceI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMReduceR4()"
!BOPI
! !IROUTINE: ESMF_VMReduce - Reduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMReduce()
  subroutine ESMF_VMReduceR4(vm, sendData, recvData, count, reduceflag, &
    root, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R4), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMReduce(vm, sendData(1), recvData(1), count, ESMF_TYPEKIND_R4, &
      reduceflag, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMReduceR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMReduceR8()"
!BOPI
! !IROUTINE: ESMF_VMReduce - Reduce 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMReduce()
  subroutine ESMF_VMReduceR8(vm, sendData, recvData, count, reduceflag, &
    root, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R8), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceflag
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Flag not implemented features
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) then
        call ESMF_LogWrite("Non-blocking not yet implemented", &
          ESMF_LOG_ERROR, &
          ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMReduce(vm, sendData(1), recvData(1), count, ESMF_TYPEKIND_R8, &
      reduceflag, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMReduceR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMScatter - Scatter data across VM
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMScatter()
!  subroutine ESMF_VMScatter<type><kind>(vm, sendData, recvData, count, root, &
!    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)
!    integer,                  intent(in)              :: count
!    integer,                  intent(in)              :: root
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that scatters contiguous data 
!   from the {\tt root} PET to all PETs across the {\tt ESMF\_VM} object
!   (including {\tt root}).
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8},
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. Only the {\tt sendData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements to be send from {\tt root} to each of the PETs. Must
!        be the same on all PETs.
!   \item[root] 
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterI4()"
!BOPI
! !IROUTINE: ESMF_VMScatter - Scatter 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterI4(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterR4()"
!BOPI
! !IROUTINE: ESMF_VMScatter - Scatter 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterR4(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R4), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterR8()"
!BOPI
! !IROUTINE: ESMF_VMScatter - Scatter 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterR8(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)
    real(ESMF_KIND_R8), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterLogical()"
!BOPI
! !IROUTINE: ESMF_VMScatter - Scatter ESMF_Logical

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterLogical(vm, sendData, recvData, count, root, &
    blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_Logical), target,      intent(in)       :: sendData(:)
    type(ESMF_Logical), target,      intent(out)      :: recvData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: root
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, root, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, root, &
        localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMScatterV - ScatterV across VM
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMScatterV()
!  subroutine ESMF_VMScatterV<type><kind>(vm, sendData, sendCounts, sendOffsets, &
!    recvData, recvCount, root, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)       :: sendData(:)
!    integer,                  intent(in)              :: sendCounts(:)
!    integer,                  intent(in)              :: sendOffsets(:)
!    <type>(ESMF_KIND_<kind>), target,   intent(out)      :: recvData(:)
!    integer,                  intent(in)              :: recvCount
!    integer,                  intent(in)              :: root
!    integer,                  intent(out),  optional  :: rc
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that scatters contiguous data 
!   from the {\tt root} PET to all PETs across the {\tt ESMF\_VM} object
!   (including {\tt root}).
!   \newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be send. Only the {\tt sendData}
!        array specified by the {\tt root} PET will be used by this method.
!   \item[sendCounts] 
!        Number of {\tt sendData} elements to be send to corresponding
!        receive PET.
!   \item[sendOffsets] 
!        Offsets in units of elements in {\tt sendData} marking the start of
!        element sequence to be send to receive PET.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[recvCount] 
!        Number of {\tt recvData} elements to receive by local PET from root
!        PET.
!   \item[root]
!        Id of the {\tt root} PET within the {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterVI4()"
!BOPI
! !IROUTINE: ESMF_VMScatterV - ScatterV 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMScatterV()
  subroutine ESMF_VMScatterVI4(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCount, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)
    integer,                  intent(in)              :: sendCounts(:)
    integer,                  intent(in)              :: sendOffsets(:)
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: root
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMScatterV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCount, ESMF_TYPEKIND_I4, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterVR4()"
!BOPI
! !IROUTINE: ESMF_VMScatterV - ScatterV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatterV()
  subroutine ESMF_VMScatterVR4(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCount, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCounts(:)
    integer,                  intent(in)              :: sendOffsets(:)
    real(ESMF_KIND_R4), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: root
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMScatterV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCount, ESMF_TYPEKIND_R4, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterVR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterVR8()"
!BOPI
! !IROUTINE: ESMF_VMScatterV - ScatterV 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatterV()
  subroutine ESMF_VMScatterVR8(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCount, root, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,   intent(in)          :: sendData(:)
    integer,                  intent(in)              :: sendCounts(:)
    integer,                  intent(in)              :: sendOffsets(:)
    real(ESMF_KIND_R8), target,   intent(out)         :: recvData(:)
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: root
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMScatterV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCount, ESMF_TYPEKIND_R8, root, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMSend - Send data to dst PET
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMSend()
!  subroutine ESMF_VMSend<type><kind>(vm, sendData, count, dst, blockingflag, &
!    commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)  
!    integer,                  intent(in)              :: count
!    integer,                  intent(in)              :: dst
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contiguous data to {\tt dst} PET within the same {\tt ESMF\_VM} object.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contiguous data array holding data to be send.
!   \item[count] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendI4()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendI4(vm, sendData, count, dst, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSend(vm, sendData(1), size, dst, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendNB(vm, sendData(1), size, dst, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendR4()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendR4(vm, sendData, count, dst, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSend(vm, sendData(1), size, dst, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendNB(vm, sendData(1), size, dst, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendR8()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send 8-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendR8(vm, sendData, count, dst, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSend(vm, sendData(1), size, dst, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendNB(vm, sendData(1), size, dst, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendLogical()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send ESMF_Logical

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendLogical(vm, sendData, count, dst, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_Logical), target,      intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSend(vm, sendData(1), size, dst, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendNB(vm, sendData(1), size, dst, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendCharacter()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send Character

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendCharacter(vm, sendData, count, dst, blockingflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    character(*),             intent(in)              :: sendData
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: dst
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: size
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count ! 1 byte
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSend(vm, sendData, size, dst, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendNB(vm, sendData, size, dst, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendCharacter
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMSendRecv - Send and Recv data to and from PETs
!
! !INTERFACE:
!  ! Private name; call using ESMF_VMSendRecv()
!  subroutine ESMF_VMSendRecv<type><kind>(vm, sendData, sendCount, dst, &
!    recvData, recvCount, src, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),            intent(in)              :: vm
!    <type>(ESMF_KIND_<kind>), target,   intent(in)    :: sendData(:)  
!    integer,                  intent(in)              :: sendCount
!    integer,                  intent(in)              :: dst
!    <type>(ESMF_KIND_<kind>), target,   intent(out)   :: recvData(:)  
!    integer,                  intent(in)              :: recvCount
!    integer,                  intent(in)              :: src
!    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
!    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
!    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Send contiguous data to {\tt dst} PET within the same {\tt ESMF\_VM} object
!   while receiving contiguous data from {\tt src} PET within the same 
!   {\tt ESMF\_VM} object. The {\tt sendData} and {\tt recvData} arrays must be
!   disjoint!\newline
!
!   This method is overloaded for: {\tt ESMF\_TYPEKIND\_I4},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8},
!   {\tt ESMF\_TYPEKIND\_LOGICAL}, {\tt ESMF\_TYPEKIND\_CHARACTER}.
!   \newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contiguous data array holding data to be send.
!   \item[sendCount] 
!        Number of elements to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contiguous data array for data to be received.
!   \item[recvCount] 
!        Number of elements to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[blockingflag]}] 
!        Flag indicating whether this call behaves blocking or non-blocking:
!        \begin{description}
!        \item[{\tt ESMF\_BLOCKING}]
!             (default) Block until local operation has completed.
!        \item[{\tt ESMF\_NONBLOCKING}]
!             Return immediately without blocking.
!        \end{description}
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt blockingflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommQueueWait()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvI4()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvI4(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    integer(ESMF_KIND_I4), target,   intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    integer(ESMF_KIND_I4), target,   intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: sendSize
    integer                 :: recvSize
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendRecvI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvR4()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv 4-byte real

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvR4(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R4), target,      intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    real(ESMF_KIND_R4), target,      intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: sendSize
    integer                 :: recvSize
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendRecvR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvR8()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv 8-byte real

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvR8(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    real(ESMF_KIND_R8), target,      intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    real(ESMF_KIND_R8), target,      intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: sendSize
    integer                 :: recvSize
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 8 ! 8 bytes
    recvSize = recvCount * 8 ! 8 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendRecvR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvLogical()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv ESMF_Logical

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvLogical(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_Logical), target,      intent(in)       :: sendData(:)  
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    type(ESMF_Logical), target,      intent(out)      :: recvData(:)  
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: sendSize
    integer                 :: recvSize
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dst, &
        recvData(1), recvSize, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendRecvLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvCharacter()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv Character

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvCharacter(vm, sendData, sendCount, dst, &
    recvData, recvCount, src, blockingflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    character(*),             intent(in)              :: sendData
    integer,                  intent(in)              :: sendCount
    integer,                  intent(in)              :: dst
    character(*),             intent(out)             :: recvData
    integer,                  intent(in)              :: recvCount
    integer,                  intent(in)              :: src
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc           
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: sendSize
    integer                 :: recvSize
    logical                 :: blocking
    type(ESMF_CommHandle)   :: localcommhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(blockingflag)) then
      if (blockingflag == ESMF_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount ! 1 byte
    recvSize = recvCount ! 1 byte
    ! Call into the C++ interface, which will sort out optional arguments.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData, sendSize, dst, &
        recvData, recvSize, src, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData, sendSize, dst, &
        recvData, recvSize, src, localcommhandle, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Check if we need to pass back the commhandle
      if (present(commhandle)) then
        commhandle = localcommhandle  ! copy the commhandle pointer back
        ! Set init code
        ESMF_INIT_SET_CREATED(commhandle)
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendRecvCharacter
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMThreadBarrier()"
!BOPI
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
!   issued the call.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMThreadBarrier(vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMThreadBarrier
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMValidate()"
!BOP
! !IROUTINE: ESMF_VMValidate - Validate VM internals

! !INTERFACE:
  subroutine ESMF_VMValidate(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)              :: vm
    integer,        intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt vm} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          Specified {\tt ESMF\_VM} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMValidate(vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMCommWait()"
!BOP
! !IROUTINE: ESMF_VMCommWait - Wait for non-blocking VM communication to complete

! !INTERFACE:
  subroutine ESMF_VMCommWait(vm, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),          intent(in)              :: vm
    type(ESMF_CommHandle),  intent(in)              :: commhandle
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Wait for non-blocking VM communication specified by the {\tt commhandle} to
!   complete.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[commhandle] 
!        Handle specifying a previously issued non-blocking communication 
!        request.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_CommHandleGetInit, commhandle, rc)

    ! Call into the C++ interface
    call c_ESMC_VMCommWait(vm, commhandle, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMCommWait
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMCommQueueWait()"
!BOP
! !IROUTINE: ESMF_VMCommQueueWait - Wait for all non-blocking VM comms to complete

! !INTERFACE:
  subroutine ESMF_VMCommQueueWait(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),          intent(in)              :: vm
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Wait for {\em all} pending non-blocking VM communication within the 
!   specified VM context to complete.\newline
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface
    call c_ESMC_VMCommQueueWait(vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMCommQueueWait
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMWtime()"
!BOP
! !IROUTINE: ESMF_VMWtime - Get floating-point number of seconds

! !INTERFACE:
  subroutine ESMF_VMWtime(time, rc)
!
! !ARGUMENTS:
    real(ESMF_KIND_R8),     intent(out)             :: time
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Get floating-point number of seconds of elapsed wall-clock time since some
!   time in the past.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[time] 
!        Time in seconds.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    call c_ESMC_VMWtime(time, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMWtime
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMWtimeDelay()"
!BOP
! !IROUTINE: ESMF_VMWtimeDelay - Delay execution

! !INTERFACE:
  subroutine ESMF_VMWtimeDelay(delay, rc)
!
! !ARGUMENTS:
    real(ESMF_KIND_R8),     intent(in)              :: delay
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Delay execution for amount of seconds.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[delay] 
!        Delay time in seconds.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    call c_ESMC_VMWtimeDelay(delay, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMWtimeDelay
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMWtimePrec()"
!BOP
! !IROUTINE: ESMF_VMWtimePrec - Timer precision as floating-point number of seconds

! !INTERFACE:
  subroutine ESMF_VMWtimePrec(prec, rc)
!
! !ARGUMENTS:
    real(ESMF_KIND_R8),     intent(out)             :: prec
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Get a run-time estimate of the timer precision as floating-point number 
!   of seconds. This is a relatively expensive call since the timer precision
!   is measured several times before the maximum is returned as the estimate.
!   The returned value is PET-specific and may differ across the VM 
!   context.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[prec] 
!        Timer precision in seconds.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    real(ESMF_KIND_R8)      :: t_1, t_2     ! timer variables
    real(ESMF_KIND_R8)      :: temp_prec    ! timer variables
    integer                 :: i

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Measuring the timer precision here by explicitly calling ESMF_WTime() will
    ! provide a better estimate than determining the precision inside the C++ 
    ! interface.
    temp_prec = 0.
    do i=1, 10
      call ESMF_VMWtime(t_1)
      t_2 = t_1
      do while (t_2 == t_1)
        call ESMF_VMWtime(t_2)
      end do
      if (t_2 - t_1 > temp_prec) temp_prec = t_2 - t_1
    end do
    prec = temp_prec

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMWtimePrec
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMInitialize()"
!BOPI
! !IROUTINE: ESMF_VMInitialize - Initialize the Global VM

! !INTERFACE:
  subroutine ESMF_VMInitialize(mpiCommunicator, rc)
!
! !ARGUMENTS:
    integer, intent(in),  optional :: mpiCommunicator
    integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Initialize the Global VM.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[{[mpiCommunicator]}] 
!        MPI communicator defining the group of processes on which the
!        ESMF application is running.
!        If not sepcified, defaults to {\tt ESMF\_COMM\_WORLD}
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMInitialize(GlobalVM, mpiCommunicator, localrc)
    ! Cannot use LogErr here because LogErr initializes _after_ VM
    if (localrc /= ESMF_SUCCESS) then
      if (present(rc)) rc = localrc
      return
    endif
    
    ! Set init code
    ESMF_INIT_SET_CREATED(GlobalVM)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMInitialize
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMFinalize()"
!BOPI
! !IROUTINE: ESMF_VMFinalize - Finalize Global VM

! !INTERFACE:
  subroutine ESMF_VMFinalize(keepMpiFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_Logical), intent(in), optional  :: keepMpiFlag
    integer, intent(out), optional            :: rc
!
! !DESCRIPTION:
!   Finalize Global VM.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[{[keepMpiFlag]}] 
!        Indicate whether MPI_Finalize() should be called or not.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMFinalize(keepMpiFlag, localrc)
    ! Cannot use LogErr here because LogErr initializes _after_ VM
    if (localrc /= ESMF_SUCCESS) then
      if (present(rc)) rc = localrc
      return
    endif
    
    ! Set init code
    ESMF_INIT_SET_DELETED(GlobalVM)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMFinalize
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAbort()"
!BOPI
! !IROUTINE: ESMF_VMAbort - Abort Global VM

! !INTERFACE:
  subroutine ESMF_VMAbort(rc)
!
! !ARGUMENTS:
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Abort Global VM.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMAbort(localrc)
    ! Cannot use LogErr here because LogErr initializes _after_ VM
    if (localrc /= ESMF_SUCCESS) then
      if (present(rc)) rc = localrc
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAbort
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMShutdown()"
!BOPI
! !IROUTINE: ESMF_VMShutdown - Shutdown a VM

! !INTERFACE:
  subroutine ESMF_VMShutdown(vm, vmplan, vm_info, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)              :: vm
    type(ESMF_VMPlan),  intent(in)              :: vmplan
    type(ESMF_Pointer), intent(inout)           :: vm_info
    integer,            intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Shutdown a VM.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMPlanGetInit, vmplan, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMShutdown(vm, vmplan, vm_info, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMShutdown
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGetInit"
!BOPI
! !IROUTINE: ESMF_VMGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_VMGetInit(vm) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_VMGetInit   
!
! !ARGUMENTS:
      type(ESMF_VM), intent(in), optional :: vm
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [vm]
!           VM object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    if (present(vm)) then
      ESMF_VMGetInit = ESMF_INIT_GET(vm)
    else
      ESMF_VMGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_VMGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_VMSetInitCreated - Set VM init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_VMSetInitCreated(vm, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(inout)           :: vm
    integer,        intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in VM object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          Specified {\tt ESMF\_VM} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(vm)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_VMSetInitCreated
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGetThis()"
!BOPI
! !IROUTINE: ESMF_VMGetThis - Internal access routine for C++ pointer

! !INTERFACE:
  subroutine ESMF_VMGetThis(vm, this, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)              :: vm
    type(ESMF_Pointer), intent(out)             :: this
    integer,            intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Internal access routine for C++ pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          Specified {\tt ESMF\_VM} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Copy C++ pointer
    this = vm%this

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_VMGetThis
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSetThis()"
!BOPI
! !IROUTINE: ESMF_VMSetThis - Set C++ pointer in VM

! !INTERFACE:
  subroutine ESMF_VMSetThis(vm, this, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(inout)           :: vm
    type(ESMF_Pointer), intent(in)              :: this
    integer,            intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Set C++ pointer in VM.
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          Specified {\tt ESMF\_VM} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Copy C++ pointer
    vm%this = this

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_VMSetThis
!------------------------------------------------------------------------------


!==============================================================================
! ESMF_VMPlan methods:
!==============================================================================


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanConstruct()"
!BOPI
! !IROUTINE: ESMF_VMPlanConstruct - Construct a default plan

! !INTERFACE:
  subroutine ESMF_VMPlanConstruct(vmplan, vm, npetlist, petlist, contextflag, &
    rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan), intent(inout)         :: vmplan
    type(ESMF_VM),     intent(in)            :: vm
    integer,           intent(in)            :: npetlist
    integer,           intent(in)            :: petlist(:)
    type(ESMF_ContextFlag), intent(in)       :: contextflag
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Construct a default plan.\newline
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanConstruct(vmplan, vm, npetlist, petlist, contextflag, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(vmplan)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMPlanConstruct
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
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
!   Destruct a vmplan.\newline
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMPlanGetInit, vmplan, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanDestruct(vmplan, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Set init code
    ESMF_INIT_SET_DELETED(vmplan)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_VMPlanDestruct
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanGetInit"
!BOPI
! !IROUTINE: ESMF_VMPlanGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_VMPlanGetInit(vmplan) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_VMPlanGetInit   
!
! !ARGUMENTS:
      type(ESMF_VMPlan), intent(in), optional :: vmplan
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [vmplan]
!           VMPlan object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    if (present(vmplan)) then
      ESMF_VMPlanGetInit = ESMF_INIT_GET(vmplan)
    else
      ESMF_VMPlanGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_VMPlanGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanGetThis()"
!BOPI
! !IROUTINE: ESMF_VMPlanGetThis - Internal access routine for C++ pointer

! !INTERFACE:
  subroutine ESMF_VMPlanGetThis(vmplan, this, rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan),  intent(in)              :: vmplan
    type(ESMF_Pointer), intent(out)             :: this
    integer,            intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Internal access routine for C++ pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[vmplan] 
!          Specified {\tt ESMF\_VMPlan} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Copy C++ pointer
    this = vmplan%this

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_VMPlanGetThis
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPlanSetThis()"
!BOPI
! !IROUTINE: ESMF_VMPlanSetThis - Set C++ pointer in VMPlan

! !INTERFACE:
  subroutine ESMF_VMPlanSetThis(vmplan, this, rc)
!
! !ARGUMENTS:
    type(ESMF_VMPlan),  intent(inout)           :: vmplan
    type(ESMF_Pointer), intent(in)              :: this
    integer,            intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Set C++ pointer in VMPlan.
!
!     The arguments are:
!     \begin{description}
!     \item[vmplan] 
!          Specified {\tt ESMF\_VMPlan} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Copy C++ pointer
    vmplan%this = this

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_VMPlanSetThis
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
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
!   Set up a MaxPEs vmplan.\newline
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMPlanGetInit, vmplan, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxPEs(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMPlanMaxPEs
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
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
!   Set up a MaxThreads vmplan.\newline
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMPlanGetInit, vmplan, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMaxThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMPlanMaxThreads
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
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
!   Set up a MinThreads vmplan.\newline
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
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMPlanGetInit, vmplan, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMPlanMinThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMPlanMinThreads
!------------------------------------------------------------------------------


!==============================================================================
! ESMF_VMId methods:
!==============================================================================


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdCompare()"
!BOPI
! !IROUTINE: ESMF_VMIdCompare - Compare two ESMF_VMId objects

! !INTERFACE:
  function ESMF_VMIdCompare(vmId1, vmId2, rc)
!
! !RETURN VALUE:
    type(ESMF_Logical) :: ESMF_VMIdCompare
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(in)            :: vmId1
    type(ESMF_VMId),   intent(in)            :: vmId2
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Compare two ESMF_VMId objects.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vmId1]
!        ESMF_VMId object 1
!   \item[vmId2]
!        ESMF_VMId object 2
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    call c_ESMC_VMIdCompare(vmId1, vmId2, ESMF_VMIdCompare, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_VMIdCompare
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdPrint()"
!BOPI
! !IROUTINE: ESMF_VMIdPrint - Print an ESMF_VMId object

! !INTERFACE:
  subroutine ESMF_VMIdPrint(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(in)            :: vmId
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Print an ESMF_VMId object.\newline
!
!   Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!   On some platforms/compilers there is a potential issue with interleaving
!   Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!   the expected order.  If this occurs, it is recommended to use the
!   standard Fortran call {\tt flush(6)} as a workaround until this issue
!   is fixed in a future release. 
!
!   The arguments are:
!   \begin{description}
!   \item[vmId] 
!        ESMF_VMId object
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    call c_ESMC_VMIdPrint(vmId, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdCreate()"
!BOPI
! !IROUTINE: ESMF_VMIdCreate - Create an ESMF_VMId object

! !INTERFACE:
  subroutine ESMF_VMIdCreate(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(inout)         :: vmId
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Create an ESMF_VMId object. This allocates memory on the C side.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vmId] 
!        ESMF_VMId object
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    call c_ESMC_VMIdCreate(vmId, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdDestroy()"
!BOPI
! !IROUTINE: ESMF_VMIdDestroy - Destroy an ESMF_VMId object

! !INTERFACE:
  subroutine ESMF_VMIdDestroy(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(inout)         :: vmId
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Destroy an ESMF_VMId object. This frees memory on the C side.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vmId] 
!        ESMF_VMId object
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    call c_ESMC_VMIdDestroy(vmId, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdDestroy
!------------------------------------------------------------------------------



! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvVMId()"
!BOPI
! !IROUTINE: ESMF_VMRecvVMId - Receive VMId

! !INTERFACE:
  subroutine ESMF_VMRecvVMId(vm, vmID, src, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_VMId),          intent(out)             :: vmId
    integer,                  intent(in)              :: src
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive {\tt ESMF\_VMId}.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[vmId] 
!        {\tt ESMF\_VMId} to be received.
!   \item[src] 
!        Id of the source PET within the {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMRecvVMId(vm, vmId, src, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvVMId
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendVMId()"
!BOPI
! !IROUTINE: ESMF_VMSendVMId - Send VMId

! !INTERFACE:
  subroutine ESMF_VMSendVMId(vm, vmID, dst, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_VMId),          intent(in)              :: vmId
    integer,                  intent(in)              :: dst
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive {\tt ESMF\_VMId}.\newline
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[vmId] 
!        {\tt ESMF\_VMId} to be send.
!   \item[dst] 
!        Id of the destination PET within the {\tt ESMF\_VM} object.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_VMSendVMId(vm, vmId, dst, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendVMId
!------------------------------------------------------------------------------


!==============================================================================
! ESMF_CommHandle methods:
!==============================================================================


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommHandleValidate()"
!BOPI
! !IROUTINE: ESMF_CommHandleValidate - Validate CommHandle internals

! !INTERFACE:
  subroutine ESMF_CommHandleValidate(commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_CommHandle),  intent(in)              :: commhandle
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt commhandle} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[commhandle] 
!          Specified {\tt ESMF\_CommHandle} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_CommHandleGetInit, commhandle, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    !todo: call c_ESMC_CommHandleValidate(commhandle, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_CommHandleValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CommHandleGetInit"
!BOPI
! !IROUTINE: ESMF_CommHandleGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_CommHandleGetInit(commhandle) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_CommHandleGetInit   
!
! !ARGUMENTS:
      type(ESMF_CommHandle), intent(in), optional :: commhandle
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [commhandle]
!           CommHandle object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    if (present(commhandle)) then
      ESMF_CommHandleGetInit = ESMF_INIT_GET(commhandle)
    else
      ESMF_CommHandleGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_CommHandleGetInit
!------------------------------------------------------------------------------




end module ESMF_VMMod


! - external subroutines used for internal ESMF purposes to circumvent 
! - circular module dependencies

subroutine f_ESMF_VMGlobalGet(localPet, petCount)
  use ESMF_VMMod
  integer, intent(out), optional  :: localPet
  integer, intent(out), optional  :: petCount
    
  call ESMF_VMGet(GlobalVM, localPet=localPet, petCount=petCount)
  
end subroutine f_ESMF_VMGlobalGet


subroutine f_ESMF_VMAbort()
  use ESMF_VMMod
    
  call ESMF_VMAbort()
  
end subroutine f_ESMF_VMAbort

