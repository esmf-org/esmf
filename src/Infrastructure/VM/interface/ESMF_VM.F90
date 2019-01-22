! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
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
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_IOUtilMod        ! ESMF I/O utility layer
      
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_CommHandle
!      
  type ESMF_CommHandle
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type
      
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_VM
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

  ! F90 class type to hold pointer to C++ object
  type ESMF_VMPlan
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

  ! F90 class type to hold pointer to C++ object
  type ESMF_VMId
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    type(ESMF_Pointer) :: this
  end type

!------------------------------------------------------------------------------
! ! Interface blocks

  interface ESMF_VMIdPrint
    module procedure ESMF_VMIdPrint_s
    module procedure ESMF_VMIdPrint_v
  end interface

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
  public operator(==)
  public operator(/=)

  public ESMF_VMAllFullReduce
  public ESMF_VMAllGather
  public ESMF_VMAllGatherV
  public ESMF_VMAllReduce
  public ESMF_VMAllToAll
  public ESMF_VMAllToAllV
  public ESMF_VMBarrier
  public ESMF_VMBroadcast
  public ESMF_VMCommWait
  public ESMF_VMCommWaitAll
  public ESMF_VMGather
  public ESMF_VMGatherV
  public ESMF_VMGet
  public ESMF_VMGetGlobal
  public ESMF_VMGetCurrent
  public ESMF_VMGetCurrentID
  public ESMF_VMGetCurrentGarbageInfo
  public ESMF_VMGetMemInfo
  public ESMF_VMIsCreated
  public ESMF_VMLogCurrentGarbageInfo
  public ESMF_VMLogMemInfo
  public ESMF_VMGetVMId
  public ESMF_VMPrint
  public ESMF_VMRecv
  public ESMF_VMReduce
  public ESMF_VMScatter
  public ESMF_VMScatterV
  public ESMF_VMSend
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
  public ESMF_VMIdCopy
  public ESMF_VMIdPrint
  public ESMF_VMIdCreate
  public ESMF_VMIdDestroy
  public ESMF_VMSendVMId
  public ESMF_VMRecvVMId
  public ESMF_VMBcastVMId

  public ESMF_CommHandleGetInit

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      "$Id$"

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
      module procedure ESMF_VMAllFullReduceI8
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
      module procedure ESMF_VMAllGatherI8
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
      module procedure ESMF_VMAllGatherVI8
      module procedure ESMF_VMAllGatherVR4
      module procedure ESMF_VMAllGatherVR8
      module procedure ESMF_VMAllGatherVCharArray
      module procedure ESMF_VMAllGatherVVMId

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
      module procedure ESMF_VMAllReduceI4S
      module procedure ESMF_VMAllReduceI8
      module procedure ESMF_VMAllReduceR4
      module procedure ESMF_VMAllReduceR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllReduce} functions.   
!EOPI 
    end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMAllToAll -- Generic interface

! !INTERFACE:
    interface ESMF_VMAllToAll

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMAllToAllI4
      module procedure ESMF_VMAllToAllI8
      module procedure ESMF_VMAllToAllR4
      module procedure ESMF_VMAllToAllR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMAllToAllV} functions.   
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
      module procedure ESMF_VMAllToAllVI8
      module procedure ESMF_VMAllToAllVR4
      module procedure ESMF_VMAllToAllVR8
      module procedure ESMF_VMAllToAllVCharArray
      module procedure ESMF_VMAllToAllVFLogical

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
      module procedure ESMF_VMBroadcastI8
      module procedure ESMF_VMBroadcastR4
      module procedure ESMF_VMBroadcastR8
      module procedure ESMF_VMBroadcastLogical
      module procedure ESMF_VMBroadcastChar
      module procedure ESMF_VMBroadcastCharArray
      module procedure ESMF_VMBroadcastCharArray2D

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
      module procedure ESMF_VMGatherI8
      module procedure ESMF_VMGatherR4
      module procedure ESMF_VMGatherR8
      module procedure ESMF_VMGatherLogical
      module procedure ESMF_VMGatherFLogical2D

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
      module procedure ESMF_VMGatherVI8
      module procedure ESMF_VMGatherVR4
      module procedure ESMF_VMGatherVR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMGatherV} functions.   
!EOPI 
    end interface

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_VMGet -- Generic interface

! !INTERFACE:
    interface ESMF_VMGet

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_VMGetDefault
      module procedure ESMF_VMGetPetLocalInfo

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMGet} functions.   
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
      module procedure ESMF_VMRecvI8
      module procedure ESMF_VMRecvR4
      module procedure ESMF_VMRecvR8
      module procedure ESMF_VMRecvLogical
      module procedure ESMF_VMRecvChar
      module procedure ESMF_VMRecvCharArray

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
      module procedure ESMF_VMReduceI8
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
      module procedure ESMF_VMScatterI8
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
      module procedure ESMF_VMScatterVI8
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
      module procedure ESMF_VMSendI8
      module procedure ESMF_VMSendR4
      module procedure ESMF_VMSendR8
      module procedure ESMF_VMSendLogical
      module procedure ESMF_VMSendChar
      module procedure ESMF_VMSendCharArray

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
      module procedure ESMF_VMSendRecvI8
      module procedure ESMF_VMSendRecvR4
      module procedure ESMF_VMSendRecvR8
      module procedure ESMF_VMSendRecvLogical
      module procedure ESMF_VMSendRecvChar

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_VMSendRecv} functions.   
!EOPI 
    end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_VMIdCreate -- Create VMId objects

! !INTERFACE:
  interface ESMF_VMIdCreate

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_VMIdCreate_s
    module procedure ESMF_VMIdCreate_v

!EOPI
  end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_VMIdDestroy -- Destroy VMId objects

! !INTERFACE:
  interface ESMF_VMIdDestroy

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_VMIdDestroy_s
    module procedure ESMF_VMIdDestroy_v

!EOPI
  end interface

!==============================================================================
      

!===============================================================================
! VMOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAssignment(=) - VM assignment
!
! !INTERFACE:
!   interface assignment(=)
!   vm1 = vm2
!
! !ARGUMENTS:
!   type(ESMF_VM) :: vm1
!   type(ESMF_VM) :: vm2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Assign vm1 as an alias to the same ESMF VM object in memory
!   as vm2. If vm2 is invalid, then vm1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[vm1]
!     The {\tt ESMF\_VM} object on the left hand side of the assignment.
!   \item[vm2]
!     The {\tt ESMF\_VM} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMOperator(==) - VM equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (vm1 == vm2) then ... endif
!             OR
!   result = (vm1 == vm2)
!
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_VM), intent(in) :: vm1
!   type(ESMF_VM), intent(in) :: vm2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether vm1 and vm2 are valid aliases to the same ESMF
!   VM object in memory. For a more general comparison of two ESMF VMs,
!   going beyond the simple alias test, the ESMF\_VMMatch() function (not yet
!   implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[vm1]
!     The {\tt ESMF\_VM} object on the left hand side of the equality
!     operation.
!   \item[vm2]
!     The {\tt ESMF\_VM} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_VMEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMOperator(/=) - VM not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (vm1 /= vm2) then ... endif
!             OR
!   result = (vm1 /= vm2)
!
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_VM), intent(in) :: vm1
!   type(ESMF_VM), intent(in) :: vm2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether vm1 and vm2 are {\it not} valid aliases to the
!   same ESMF VM object in memory. For a more general comparison of two ESMF
!   VMs, going beyond the simple alias test, the ESMF\_VMMatch() function
!   (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[vm1]
!     The {\tt ESMF\_VM} object on the left hand side of the non-equality
!     operation.
!   \item[vm2]
!     The {\tt ESMF\_VM} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_VMNE

  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMEQ()"
!BOPI
! !IROUTINE:  ESMF_VMEQ - Compare two VMs for equality
!
! !INTERFACE:
  function ESMF_VMEQ(vm1, vm2)
! 
! !RETURN VALUE:
    logical :: ESMF_VMEQ

! !ARGUMENTS:
    type(ESMF_VM), intent(in) :: vm1
    type(ESMF_VM), intent(in) :: vm2

! !DESCRIPTION:
!   Test if both {\tt vm1} and {\tt vm2} alias the same ESMF VM 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE init1, init2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: using Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    init1 = ESMF_VMGetInit(vm1)
    init2 = ESMF_VMGetInit(vm2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (init1 .eq. ESMF_INIT_CREATED .and. &
      init2 .eq. ESMF_INIT_CREATED) then
      ESMF_VMEQ = vm1%this == vm2%this
    else
      ESMF_VMEQ = ESMF_FALSE
    endif

  end function ESMF_VMEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMNE()"
!BOPI
! !IROUTINE:  ESMF_VMNE - Compare two VMs for non-equality
!
! !INTERFACE:
  function ESMF_VMNE(vm1, vm2)
! 
! !RETURN VALUE:
    logical :: ESMF_VMNE

! !ARGUMENTS:
    type(ESMF_VM), intent(in) :: vm1
    type(ESMF_VM), intent(in) :: vm2

! !DESCRIPTION:
!   Test if both {\tt vm1} and {\tt vm2} alias the same ESMF VM 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_VMNE = .not.ESMF_VMEQ(vm1, vm2)

  end function ESMF_VMNE
!-------------------------------------------------------------------------------
      
        
! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllFullReduce - Fully reduce data across VM, result on all PETs
!
! !INTERFACE:
!  subroutine ESMF_VMAllFullReduce(vm, sendData, recvData, &
!    count, reduceflag, syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    <type>(ESMF_KIND_<kind>),         intent(out)           :: recvData
!    integer,                          intent(in)            :: count
!    type(ESMF_Reduce_Flag),           intent(in)            :: reduceflag
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that reduces a contiguous data 
!   array of <type><kind> across the {\tt ESMF\_VM} object 
!   into a single value of the same <type><kind>. The result is
!   returned on all PETs. Different reduction operations can be specified.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt syncflag = ESMF\_SYNC\_NONBLOCKING}, error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Single data variable to be received. All PETs must specify a
!        valid result variable.
!   \item[count] 
!        Number of elements in sendData. Allowed to be different across the 
!        PETs, as long as {\tt count} > 0.
!   \item[reduceflag] 
!        Reduction operation. See section \ref{const:reduce} for a list of 
!        valid reduce operations.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
    reduceflag, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I4),         intent(out)           :: recvData
    integer,                       intent(in)            :: count
    type(ESMF_Reduce_Flag),        intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif
    
    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllFullReduce(vm, sendData(1), recvData, count, &
        ESMF_TYPEKIND_I4, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllFullReduceI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllFullReduceI8()"
!BOPI
! !IROUTINE: ESMF_VMAllFullReduce - AllFullReduce 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllFullReduce()
  subroutine ESMF_VMAllFullReduceI8(vm, sendData, recvData, count, &
    reduceflag, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I8),         intent(out)           :: recvData
    integer,                       intent(in)            :: count
    type(ESMF_Reduce_Flag),        intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif
    
    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllFullReduce(vm, sendData(1), recvData, count, &
        ESMF_TYPEKIND_I8, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllFullReduceI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllFullReduceR4()"
!BOPI
! !IROUTINE: ESMF_VMAllFullReduce - AllFullReduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllFullReduce()
  subroutine ESMF_VMAllFullReduceR4(vm, sendData, recvData, count, &
    reduceflag, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R4),         intent(out)           :: recvData
    integer,                    intent(in)            :: count
    type(ESMF_Reduce_Flag),     intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllFullReduce(vm, sendData(1), recvData, count, &
        ESMF_TYPEKIND_R4, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

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
    reduceflag, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R8),         intent(out)           :: recvData
    integer,                    intent(in)            :: count
    type(ESMF_Reduce_Flag),     intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllFullReduce(vm, sendData(1), recvData, count, &
        ESMF_TYPEKIND_R8, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllFullReduceR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllGather - Gather data across VM, result on all PETs
!
! !INTERFACE:
!  subroutine ESMF_VMAllGather(vm, sendData, recvData, count, &
!    syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: count
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object into an array on all PETs.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid {\tt recvData} argument.
!   \item[count] 
!        Number of elements to be gathered from each PET. Must be the
!        same on all PETs.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData(1), recvData(1), size, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData(1), recvData(1), size, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#define ESMF_METHOD "ESMF_VMAllGatherI8()"
!BOPI
! !IROUTINE: ESMF_VMAllGather - AllGather 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGather()
  subroutine ESMF_VMAllGatherI8(vm, sendData, recvData, count, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData(1), recvData(1), size, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData(1), recvData(1), size, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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

  end subroutine ESMF_VMAllGatherI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherR4()"
!BOPI
! !IROUTINE: ESMF_VMAllGather - AllGather 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGather()
  subroutine ESMF_VMAllGatherR4(vm, sendData, recvData, count, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData, recvData, size, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData, recvData, size, localcommhandle, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData(1), recvData(1), size, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData(1), recvData(1), size, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_Logical), target, intent(in)            :: sendData(:)
    type(ESMF_Logical), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMAllGather(vm, sendData(1), recvData(1), size, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMAllGatherNB(vm, sendData(1), recvData(1), size, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!  subroutine ESMF_VMAllGatherV(vm, sendData, sendCount, &
!    recvData, recvCounts, recvOffsets, syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    integer,                          intent(in)            :: sendCount
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: recvCounts(:)
!    integer,                          intent(in)            :: recvOffsets(:)
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object into an array on all PETs.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt syncflag = ESMF\_SYNC\_NONBLOCKING}, error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[sendCount] 
!        Number of {\tt sendData} elements to send from local PET to all other
!        PETs.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid {\tt recvData} argument.
!   \item[recvCounts] 
!        Number of {\tt recvData} elements to be received from corresponding
!        source PET.
!   \item[recvOffsets] 
!        Offsets in units of elements in {\tt recvData} marking the start of
!        element sequence to be received from source PET.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
    recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCounts(:)
    integer,                       intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllGatherV(vm, sendData, sendCount, &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherVI8()"
!BOPI
! !IROUTINE: ESMF_VMAllGatherV - AllGatherV 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGatherV()
  subroutine ESMF_VMAllGatherVI8(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCounts(:)
    integer,                       intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllGatherV(vm, sendData, sendCount, &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherVR4()"
!BOPI
! !IROUTINE: ESMF_VMAllGatherV - AllGatherV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGatherV()
  subroutine ESMF_VMAllGatherVR4(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCount
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllGatherV(vm, sendData, sendCount, &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCount
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllGatherV(vm, sendData, sendCount, &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherVCharArray()"
!BOPI
! !IROUTINE: ESMF_VMAllGatherV - AllGatherV CHARACTER array

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGatherV()
  subroutine ESMF_VMAllGatherVCharArray(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    character(*),       target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCount
    character(*),       target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllGatherV(vm,  &
        sendData, sendCount, &
        recvData, recvCounts(1), recvOffsets(1),  &
        ESMF_TYPEKIND_CHARACTER, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVCharArray
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllGatherVVMId()"
!BOPI
! !IROUTINE: ESMF_VMAllGatherV - AllGatherV VMIds

! !INTERFACE:
  ! Private name; call using ESMF_VMAllGatherV()
  subroutine ESMF_VMAllGatherVVMId(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_VMId),    target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCount
    type(ESMF_VMId),    target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="- non-blocking mode not yet implemented", &
            ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllGatherVVMId (vm,  &
        sendData, sendCount,  &
        recvData, recvCounts, recvOffsets,  &
        localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllGatherVVMId
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllReduce - Reduce data across VM, result on all PETs
!
! !INTERFACE:
!  subroutine ESMF_VMAllReduce(vm, sendData, recvData, count, &
!    reduceflag, syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: count
!    type(ESMF_Reduce_Flag),           intent(in)            :: reduceflag
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that reduces a contiguous data 
!   array across the {\tt ESMF\_VM} object into a contiguous data array of the
!   same <type><kind>. The result array is returned on all PETs. 
!   Different reduction operations can be specified.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt syncflag = ESMF\_SYNC\_NONBLOCKING}, error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid {\tt recvData} argument.
!   \item[count] 
!        Number of elements in sendData and recvData. Must be the same on all
!        PETs.
!   \item[reduceflag] 
!        Reduction operation. See section \ref{const:reduce} for a list of 
!        valid reduce operations.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    type(ESMF_Reduce_Flag),        intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_I4, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllReduceI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceI4S()"
!BOPI
! !IROUTINE: ESMF_VMAllReduce - AllReduce 4-byte integers, scalar version

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceI4S(vm, sendData, recvData, reduceflag, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData
    type(ESMF_Reduce_Flag),        intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllReduce(vm, sendData, recvData, 1, ESMF_TYPEKIND_I4, &
      reduceflag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllReduceI4S
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceI8()"
!BOPI
! !IROUTINE: ESMF_VMAllReduce - AllReduce 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceI8(vm, sendData, recvData, count, reduceflag, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    type(ESMF_Reduce_Flag),        intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_I8, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllReduceI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllReduceR4()"
!BOPI
! !IROUTINE: ESMF_VMAllReduce - AllReduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllReduce()
  subroutine ESMF_VMAllReduceR4(vm, sendData, recvData, count, reduceflag, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    type(ESMF_Reduce_Flag),     intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_R4, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

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
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    type(ESMF_Reduce_Flag),     intent(in)            :: reduceflag
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMAllReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_R8, reduceflag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllReduceR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllToAll - AllToAll communications across VM
!
! !INTERFACE:
!  subroutine ESMF_VMAllToAll(vm, sendData, sendCount, &
!    recvData, recvCount, syncflag, &
!    commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    integer,                          intent(in)            :: sendCount
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: recvCount
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.3.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs a total exchange
!   operation, sending pieces of the contiguous data buffer {\tt sendData} to
!   all other PETs while receiving data into the contiguous data buffer
!   {\tt recvData} from all other PETs.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt syncflag = ESMF\_SYNC\_NONBLOCKING}, error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[sendCount] 
!        Number of {\tt sendData} elements to send from local PET to
!        each destination PET.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid {\tt recvData} argument.
!   \item[recvCount] 
!        Number of {\tt recvData} elements to be received by local PET from
!        each source PET.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
!        communication calls of the VM context.
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllI4()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAll 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAll()
  subroutine ESMF_VMAllToAllI4(vm, sendData, sendCount, &
    recvData, recvCount, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCount
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAll(vm, sendData, sendCount, &
      recvData, recvCount, ESMF_TYPEKIND_I4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllI8()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAll 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAll()
  subroutine ESMF_VMAllToAllI8(vm, sendData, sendCount, &
    recvData, recvCount, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCount
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAll(vm, sendData, sendCount, &
      recvData, recvCount, ESMF_TYPEKIND_I8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllR4()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAll 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAll()
  subroutine ESMF_VMAllToAllR4(vm, sendData, sendCount, &
    recvData, recvCount, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    real(ESMF_KIND_R4),    target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    real(ESMF_KIND_R4),    target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCount
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAll(vm, sendData, sendCount, &
      recvData, recvCount, ESMF_TYPEKIND_R4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllR8()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAll 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAll()
  subroutine ESMF_VMAllToAllR8(vm, sendData, sendCount, &
    recvData, recvCount, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    real(ESMF_KIND_R8),    target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    real(ESMF_KIND_R8),    target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCount
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAll(vm, sendData, sendCount, &
      recvData, recvCount, ESMF_TYPEKIND_R8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV communications across VM
!
! !INTERFACE:
!  subroutine ESMF_VMAllToAllV(vm, sendData, sendCounts, &
!    sendOffsets, recvData, recvCounts, recvOffsets, syncflag, &
!    commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    integer,                          intent(in)            :: sendCounts(:)
!    integer,                          intent(in)            :: sendOffsets(:)
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: recvCounts(:)
!    integer,                          intent(in)            :: recvOffsets(:)
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that performs a total exchange
!   operation, sending pieces of the contiguous data buffer {\tt sendData} to
!   all other PETs while receiving data into the contiguous data buffer
!   {\tt recvData} from all other PETs.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt syncflag = ESMF\_SYNC\_NONBLOCKING}, error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[sendCounts] 
!        Number of {\tt sendData} elements to send from local PET to
!        destination PET.
!   \item[sendOffsets] 
!        Offsets in units of elements in {\tt sendData} marking to start of
!        element sequence to be sent from local PET to destination PET.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid {\tt recvData} argument.
!   \item[recvCounts] 
!        Number of {\tt recvData} elements to be received by local PET from
!        source PET.
!   \item[recvOffsets] 
!        Offsets in units of elements in {\tt recvData} marking to start of
!        element sequence to be received by local PET from source PET.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
    recvData, recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCounts(:)
    integer,                       intent(in)            :: sendOffsets(:)
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCounts(:)
    integer,                       intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAllV(vm, sendData, sendCounts(1), sendOffsets(1), &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllVI8()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVI8(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCounts(:)
    integer,                       intent(in)            :: sendOffsets(:)
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCounts(:)
    integer,                       intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAllV(vm, sendData, sendCounts(1), sendOffsets(1), &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllVR4()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVR4(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCounts(:)
    integer,                    intent(in)            :: sendOffsets(:)
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAllV(vm, sendData, sendCounts(1), sendOffsets(1), &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    recvData, recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCounts(:)
    integer,                    intent(in)            :: sendOffsets(:)
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAllV(vm, sendData, sendCounts(1), sendOffsets(1), &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllVCharArray()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV Character array

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVCharArray(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    character(*),       target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCounts(:)
    integer,                    intent(in)            :: sendOffsets(:)
    character(*),       target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!
!   Note that since this is for character data type, the values in the offsets
!   arrays must already have taken into account the length of the strings.
!   That is, this method does not multiply sendOffsets by len(sendData) or
!   recvOffsets by len(recvData).
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAllV(vm, sendData, sendCounts(1), sendOffsets(1), &
      recvData, recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_CHARACTER, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVCharArray
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllVFLogical()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV Fortran logicals

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVFLogical(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCounts, recvOffsets, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    logical,            target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCounts(:)
    integer,                    intent(in)            :: sendOffsets(:)
    logical,            target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    call c_ESMC_VMAllToAllV(vm,  &
        sendData, sendCounts(1), sendOffsets(1), &
        recvData, recvCounts(1), recvOffsets(1), &
        ESMF_TYPEKIND_LOGICAL, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVFLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBarrier()"
!BOP
! !IROUTINE: ESMF_VMBarrier - VM wide barrier

! !INTERFACE:
  subroutine ESMF_VMBarrier(vm, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,        intent(out), optional :: rc           
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that blocks calling PET until
!   all PETs of the VM context have issued the call.
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

    ! Call into the C++ interface.
    call c_ESMC_VMBarrier(vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!  subroutine ESMF_VMBroadcast(vm, bcstData, count, rootPet, &
!    syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(inout)         :: bcstData(:)
!    integer,                          intent(in)            :: count
!    integer,                          intent(in)            :: rootPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that broadcasts a contiguous 
!   data array from {\tt rootPet} to all other PETs of the {\tt ESMF\_VM}
!   object.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}, 
!   {\tt ESMF\_TYPEKIND\_CHARACTER}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[bcstData]
!        Contiguous data array. On {\tt rootPet} {\tt bcstData} holds data that
!        is to be broadcasted to all other PETs. On all other PETs 
!        {\tt bcstData} is used to receive the broadcasted data.
!   \item[count] 
!        Number of elements in {/bcstData}. Must be the same on all PETs.
!   \item[rootPet] 
!        PET that holds data that is being broadcast.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
  subroutine ESMF_VMBroadcastI4(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(inout)         :: bcstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData(1), size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastI8()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastI8(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(inout)         :: bcstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count * 8 ! 8 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData(1), size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastR4()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastR4(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(inout)         :: bcstData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData(1), size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
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
  subroutine ESMF_VMBroadcastR8(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(inout)         :: bcstData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count * 8 ! 8 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData(1), size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
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
  subroutine ESMF_VMBroadcastLogical(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_Logical), target, intent(inout)         :: bcstData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData(1), size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData(1), size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastChar()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast ESMF_Character

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastChar(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*), target,    intent(inout)         :: bcstData
    integer,                 intent(in)            :: count
    integer,                 intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData, size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData, size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastChar
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastCharArray()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast ESMF_CharArray

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastCharArray(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*), target,    intent(inout)         :: bcstData(:)
    integer,                 intent(in)            :: count
    integer,                 intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData, size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData, size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastCharArray
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBroadcastCharArray2D()"
!BOPI
! !IROUTINE: ESMF_VMBroadcast - Broadcast 2D ESMF_CharacterArray

! !INTERFACE:
  ! Private name; call using ESMF_VMBroadcast()
  subroutine ESMF_VMBroadcastCharArray2D(vm, bcstData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*), target,    intent(inout)         :: bcstData(:,:)
    integer,                 intent(in)            :: count
    integer,                 intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      size = count
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMBroadcast(vm, bcstData, size, rootPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMBroadcastNB(vm, bcstData, size, rootPet, localcommhandle, &
          localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBroadcastCharArray2D
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMCommWait()"
!BOP
! !IROUTINE: ESMF_VMCommWait - Wait for non-blocking VM communication to complete

! !INTERFACE:
  subroutine ESMF_VMCommWait(vm, commhandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),         intent(in)            :: vm
    type(ESMF_CommHandle), intent(in)            :: commhandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,               intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Wait for non-blocking VM communication specified by the {\tt commhandle} to
!   complete.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMCommWait
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMCommWaitAll()"
!BOP
! !IROUTINE: ESMF_VMCommWaitAll - Wait for all non-blocking VM comms to complete

! !INTERFACE:
  subroutine ESMF_VMCommWaitAll(vm, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,       intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Wait for {\em all} pending non-blocking VM communication within the 
!   specified VM context to complete.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMCommWaitAll
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMGather - Gather data from across VM
!
! !INTERFACE:
!  subroutine ESMF_VMGather(vm, sendData, recvData, count, rootPet, &
!    syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: count
!    integer,                          intent(in)            :: rootPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object (including {\tt rootPet}) into an
!   array on {\tt rootPet}.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contiguous data array for data to be received. Only the {\tt recvData}
!        array specified by the {\tt rootPet} will be used by this method.
!   \item[count] 
!        Number of elements to be sent from each PET to {\tt rootPet}. Must be
!        the same on all PETs.
!   \item[rootPet] 
!        PET on which data is gathereds.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
  subroutine ESMF_VMGatherI4(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#define ESMF_METHOD "ESMF_VMGatherI8()"
!BOPI
! !IROUTINE: ESMF_VMGather - Gather 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherI8(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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

  end subroutine ESMF_VMGatherI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherR4()"
!BOPI
! !IROUTINE: ESMF_VMGather - Gather 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherR4(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMGatherR8(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMGatherLogical(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_Logical), target, intent(in)            :: sendData(:)
    type(ESMF_Logical), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherFLogical2D()"
!BOPI
! !IROUTINE: ESMF_VMGather - Gather Fortran logical

! !INTERFACE:
  ! Private name; call using ESMF_VMGather()
  subroutine ESMF_VMGatherFLogical2D(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    logical, target,            intent(in)            :: sendData(:)
    logical, target,            intent(out)           :: recvData(:,:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMGather(vm, sendData, recvData, size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMGatherNB(vm, sendData, recvData, size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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

  end subroutine ESMF_VMGatherFLogical2D
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMGatherV - GatherV data from across VM
!
! !INTERFACE:
!  subroutine ESMF_VMGatherV(vm, sendData, sendCount, recvData, &
!    recvCounts, recvOffsets, rootPet, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    integer,                          intent(in)            :: sendCount
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: recvCounts(:)
!    integer,                          intent(in)            :: recvOffsets(:)
!    integer,                          intent(in)            :: rootPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that gathers contiguous data 
!   from all PETs of an {\tt ESMF\_VM} object into an array on rootPet.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt syncflag = ESMF\_SYNC\_NONBLOCKING}, error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[sendCount] 
!        Number of {\tt sendData} elements to send from local PET to all other
!        PETs.
!   \item[recvData] 
!        Contiguous data array for data to be received. Only the {\tt recvData}
!        array specified by the {\tt rootPet} will be used by this method.
!   \item[recvCounts] 
!        Number of {\tt recvData} elements to be received from corresponding
!        source PET.
!   \item[recvOffsets] 
!        Offsets in units of elements in {\tt recvData} marking the start of
!        element sequence to be received from source PET.
!   \item[rootPet]
!        PET on which data is gathered.
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
    recvCounts, recvOffsets, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCounts(:)
    integer,                       intent(in)            :: recvOffsets(:)
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMGatherV(vm, sendData, sendCount, recvData, &
      recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I4, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherVI8()"
!BOPI
! !IROUTINE: ESMF_VMGatherV - GatherV 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMGatherV()
  subroutine ESMF_VMGatherVI8(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCount
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCounts(:)
    integer,                       intent(in)            :: recvOffsets(:)
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMGatherV(vm, sendData, sendCount, recvData, &
      recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_I8, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherVI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGatherVR4()"
!BOPI
! !IROUTINE: ESMF_VMGatherV - GatherV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMGatherV()
  subroutine ESMF_VMGatherVR4(vm, sendData, sendCount, recvData, &
    recvCounts, recvOffsets, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCount
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMGatherV(vm, sendData, sendCount, recvData, &
      recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R4, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    recvCounts, recvOffsets, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCount
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCounts(:)
    integer,                    intent(in)            :: recvOffsets(:)
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMGatherV(vm, sendData, sendCount, recvData, &
      recvCounts(1), recvOffsets(1), ESMF_TYPEKIND_R8, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGatherVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGetDefault()"
!BOP
! !IROUTINE: ESMF_VMGet - Get object-wide information from a VM

! !INTERFACE:
  ! Private name; call using ESMF_VMGet()
  recursive subroutine ESMF_VMGetDefault(vm, keywordEnforcer, localPet, &
    petCount, peCount, ssiCount, ssiMinPetCount, ssiMaxPetCount, &
    ssiLocalPetCount, mpiCommunicator, pthreadsEnabledFlag, openMPEnabledFlag, &
    ssiSharedMemoryEnabledFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: localPet
    integer,            intent(out), optional :: petCount
    integer,            intent(out), optional :: peCount
    integer,            intent(out), optional :: ssiCount
    integer,            intent(out), optional :: ssiMinPetCount
    integer,            intent(out), optional :: ssiMaxPetCount
    integer,            intent(out), optional :: ssiLocalPetCount
    integer,            intent(out), optional :: mpiCommunicator
    logical,            intent(out), optional :: pthreadsEnabledFlag
    logical,            intent(out), optional :: openMPEnabledFlag
    logical,            intent(out), optional :: ssiSharedMemoryEnabledFlag
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[8.0.0] Added arguments {\tt ssiCount}, {\tt ssiMinPetCount}, 
!   {\tt ssiMaxPetCount}, and {\tt ssiLocalPetCount} to provide access 
!   to information about how the VM is mapped across the single system images
!   (SSI) -- typically synonymous to nodes -- of the compute environment. This
!   information is useful when constructing custom petLists. \newline
!   Added argument {\tt ssiSharedMemoryEnabledFlag} that allows the user to 
!   query whether ESMF was compiled with support for shared memory 
!   access between PETs on the same SSI.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Get internal information about the specified {\tt ESMF\_VM} object.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        Queried {\tt ESMF\_VM} object.
!   \item[{[localPet]}]
!        Upon return this holds the id of the PET that issued this call.
!   \item[{[petCount]}]
!        Upon return this holds the number of PETs running under {\tt vm}.
!   \item[{[peCount]}]
!        Upon return this holds the number of PEs referenced by {\tt vm}.
!   \item[{[ssiCount]}]
!        Upon return this holds the number of single system images referenced 
!        by {\tt vm}.
!   \item[{[ssiMinPetCount]}]
!        Upon return this holds the smallest number of PETs running in the same
!        single system images under {\tt vm}.
!   \item[{[ssiMaxPetCount]}]
!        Upon return this holds the largest number of PETs running in the same
!        single system images under {\tt vm}.
!   \item[{[ssiLocalPetCount]}]
!        Upon return this holds the number of PETs running in the same
!        single system as {\tt localPet}.
!   \item[{[mpiCommunicator]}]
!        Upon return this holds the MPI intra-communicator used by the 
!        specified {\tt ESMF\_VM} object. This communicator may be used for
!        user-level MPI communications. It is recommended that the user
!        duplicates the communicator via {\tt MPI\_Comm\_Dup()} in order to
!        prevent any interference with ESMF communications.
!   \item[{[pthreadsEnabledFlag]}]
!        \begin{description}
!        \item[{\tt .TRUE.}]
!             ESMF has been compiled with Pthreads.
!        \item[{\tt .FALSE.}]
!             ESMF has {\em not} been compiled with Pthreads.
!        \end{description}
!   \item[{[openMPEnabledFlag]}]
!        \begin{description}
!        \item[{\tt .TRUE.}]
!             ESMF has been compiled with OpenMP.
!        \item[{\tt .FALSE.}]
!             ESMF has {\em not} been compiled with OpenMP.
!        \end{description}
!   \item[{[ssiSharedMemoryEnabledFlag]}]
!        \begin{description}
!        \item[{\tt .TRUE.}]
!             ESMF has been compiled to support shared memory access
!             between PETs that are on the same single system image (SSI).
!        \item[{\tt .FALSE.}]
!             ESMF has {\em not} been compiled to support shared memory access
!             between PETs that are on the same single system image (SSI).
!        \end{description}
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    type(ESMF_Logical)      :: pthreadsEnabledFlagArg         ! helper variable
    type(ESMF_Logical)      :: openMPEnabledFlagArg           ! helper variable
    type(ESMF_Logical)      :: ssiSharedMemoryEnabledFlagArg  ! helper variable
    integer                 :: localrc  ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMGet(vm, localPet, petCount, peCount, ssiCount, &
      ssiMinPetCount, ssiMaxPetCount, ssiLocalPetCount, mpiCommunicator, &
      pthreadsEnabledFlagArg, openMPEnabledFlagArg, &
      ssiSharedMemoryEnabledFlagArg, localrc)
    if (present (pthreadsEnabledFlag))  &
      pthreadsEnabledFlag = pthreadsEnabledFlagArg
    if (present (openMPEnabledFlag))  &
      openMPEnabledFlag = openMPEnabledFlagArg
    if (present (ssiSharedMemoryEnabledFlag))  &
      ssiSharedMemoryEnabledFlag = ssiSharedMemoryEnabledFlagArg
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetDefault
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMGetPetLocalInfo()"
!BOP
! !IROUTINE: ESMF_VMGet - Get PET-local VM information

! !INTERFACE:
  ! Private name; call using ESMF_VMGet()
  subroutine ESMF_VMGetPetLocalInfo(vm, pet, keywordEnforcer, peCount, &
    accDeviceCount, ssiId, threadCount, threadId, vas, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
    integer,       intent(in)            :: pet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,       intent(out), optional :: peCount
    integer,       intent(out), optional :: accDeviceCount
    integer,       intent(out), optional :: ssiId
    integer,       intent(out), optional :: threadCount
    integer,       intent(out), optional :: threadId
    integer,       intent(out), optional :: vas
    integer,       intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \item\apiStatusModifiedSinceVersion{5.2.0r}
! \begin{description}
! \item[7.0.0] Added argument {\tt accDeviceCount}.
!   The argument provides access to the number of available accelerator devices.
! \end{description}
! \end{itemize}
!
! !DESCRIPTION:
!   Get internal information about a specific PET within an {\tt ESMF\_VM} 
!   object.
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
!   \item[{[accDeviceCount]}]
!        Upon return this holds the number of accelerated devices accessible
!        from the specified PET in the {\tt ESMF\_VM} object.
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

    ! Call into the C++ interface.
    call c_ESMC_VMGetPETLocalInfo(vm, pet, peCount, accDeviceCount, &
      ssiId, threadCount, threadId, vas, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetPetLocalInfo
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMGetGlobal - Get Global VM

! !INTERFACE:
  subroutine ESMF_VMGetGlobal(vm, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(out)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,       intent(out), optional  :: rc           
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Get the global {\tt ESMF\_VM} object. This is the VM object
!   that is created during {\tt ESMF\_Initialize()} and is the ultimate
!   parent of all VM objects in an ESMF application. It is identical to the VM
!   object returned by {\tt ESMF\_Initialize(..., vm=vm, ...)}.
!   \end{sloppypar}
!
!   The {\tt ESMF\_VMGetGlobal()} call provides access to information about the
!   global execution context via the global VM. This call is necessary because
!   ESMF does not created a global ESMF Component during
!   {\tt ESMF\_Initialize()} that could be queried for information about
!   the global execution context of an ESMF application.
!
!   Usage of {\tt ESMF\_VMGetGlobal()} from within Component code is
!   strongly discouraged. ESMF Components should only access their own VM
!   objects through Component methods. Global information, if required by
!   the Component user code, should be passed down to the Component from the 
!   driver through the Component calling interface.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!     Upon return this holds the {\tt ESMF\_VM} object of the global execution 
!     context.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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
  subroutine ESMF_VMGetCurrent(vm, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(out)           :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,       intent(out), optional :: rc           
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   \begin{sloppypar}
!   Get the {\tt ESMF\_VM} object of the current execution context. Calling
!   {\tt ESMF\_VMGetCurrent()} within an ESMF Component, will return the
!   same VM object as
!   {\tt ESMF\_GridCompGet(..., vm=vm, ...)} or
!   {\tt ESMF\_CplCompGet(..., vm=vm, ...)}. 
!   \end{sloppypar}
! 
!   The main purpose of providing {\tt ESMF\_VMGetCurrent()} is to simplify ESMF
!   adoption in legacy code. Specifically, code that uses {\tt MPI\_COMM\_WORLD}
!   deep within its calling tree can easily be modified to use the correct MPI
!   communicator of the current ESMF execution context. The advantage is that
!   these modifications are very local, and do not require wide reaching
!   interface changes in the legacy code to pass down the ESMF component object,
!   or the MPI communicator.
!
!   The use of {\tt ESMF\_VMGetCurrent()} is strongly discouraged in newly
!   written Component code. Instead, the ESMF Component object should be used as
!   the appropriate container of ESMF context information. This object should be
!   passed between the subroutines of a Component, and be queried for any
!   Component specific information.
!
!   Outside of a Component context, i.e. within the driver context, the call
!   to {\tt ESMF\_VMGetCurrent()} is identical to {\tt ESMF\_VMGetGlobal()}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!     Upon return this holds the {\tt ESMF\_VM} object of the current execution
!     context.
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

    ! Call into the C++ interface.
    call c_ESMC_VMGetCurrent(vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    type(ESMF_VMId), intent(out)           :: vmId
    integer,         intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_VMId} of the current execution context.
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

    ! Call into the C++ interface.
    call c_ESMC_VMGetCurrentID(vmId, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetCurrentID
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_VMGetCurrentGarbageInfo - Get Current VMId

! !INTERFACE:
  subroutine ESMF_VMGetCurrentGarbageInfo(fobjCount, objCount, rc)
!
! !ARGUMENTS:
    integer, intent(in)             :: fobjCount
    integer, intent(in)             :: objCount
    integer, intent(out), optional  :: rc           
!
! !DESCRIPTION:
!   Get garbage collection information of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[fobjCount] 
!     Upon return this holds the number of Fortran objects held by the
!     ESMF garbage collection for the current context.
!   \item[objCount] 
!     Upon return this holds the number of Base objects
!     held by the ESMF garbage collection for the current context.
!     Note that {\em every} C++ object in the garbage collection has a Base 
!     object. However, for Fortran objects, {\em most} have also a Base object,
!     but not all of them. E.g. no Base object is created for GeomBase objects!
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

    ! Call into the C++ interface.
    call c_esmc_vmgetcurrentgarbageinfo(fobjCount, objCount, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetCurrentGarbageInfo
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_VMGetMemInfo - Get memory info for this PET

! !INTERFACE:
  subroutine ESMF_VMGetMemInfo(virtMemPet, physMemPet, rc)
!
! !ARGUMENTS:
    integer, intent(in)             :: virtMemPet
    integer, intent(in)             :: physMemPet
    integer, intent(out), optional  :: rc           
!
! !DESCRIPTION:
!   Get memory info from the system for this PET.
!
!   The arguments are:
!   \begin{description}
!   \item[virtMemPet] 
!     Virtual memory in kB currently used by this PET.
!   \item[physMemPet] 
!     Physical memory in kB currently used by this PET.
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

    ! Call into the C++ interface.
    call c_esmc_vmgetmeminfo(virtMemPet, physMemPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetMemInfo
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIsCreated()"
!BOP
! !IROUTINE: ESMF_VMIsCreated - Check whether a VM object has been created

! !INTERFACE:
  function ESMF_VMIsCreated(vm, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_VMIsCreated
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,       intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt vm} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[vm]
!     {\tt ESMF\_VM} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_VMIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_VMGetInit(vm)==ESMF_INIT_CREATED) &
      ESMF_VMIsCreated = .true.
  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_VMLogCurrentGarbageInfo - Log garbage collection info

! !INTERFACE:
  subroutine ESMF_VMLogCurrentGarbageInfo(prefix, rc)
!
! !ARGUMENTS:
    character (len=*),    intent(in),   optional  :: prefix
    integer, intent(out),               optional  :: rc           
!
! !DESCRIPTION:
!   Log memory info from the system for this PET.
!
!   The arguments are:
!   \begin{description}
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

    ! Call into the C++ interface.
    call c_esmc_vmlogcurrentgarbageinfo(prefix, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMLogCurrentGarbageInfo
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_VMLogMemInfo - Log memory info for this PET

! !INTERFACE:
  subroutine ESMF_VMLogMemInfo(prefix, log, rc)
!
! !ARGUMENTS:
    character (len=*),    intent(in),   optional  :: prefix
    type(ESMF_Log),       intent(inout),optional  :: log
    integer, intent(out),               optional  :: rc           
!
! !DESCRIPTION:
!   Log memory info from the system for this PET.
!
!   The arguments are:
!   \begin{description}
!   \item [{[prefix]}]
!     String to prefix the memory info message. Default is no prefix.
!   \item [{[log]}] !TODO: BROKEN!!!
!     {\tt ESMF\_Log} object that can be used instead of the default Log.
!     Default is to use the default log.
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

    ! Call into the C++ interface.
    call c_esmc_vmlogmeminfo(prefix, log, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMLogMemInfo
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
!BOPI
! !IROUTINE: ESMF_VMGetVMId - Get VMId

! !INTERFACE:
  subroutine ESMF_VMGetVMId(vm, vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),   intent(in)            :: vm
    type(ESMF_VMId), intent(out)           :: vmId
    integer,         intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_VMId} of the {\tt ESMF\_VM} object.
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

    ! Call into the C++ interface.
    call c_ESMC_VMGetVMId(vm, vmId, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMGetVMId
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMPrint()"
!BOP
! !IROUTINE: ESMF_VMPrint - Print VM information

! !INTERFACE:
  subroutine ESMF_VMPrint(vm, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),  intent(in)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,        intent(out), optional :: rc           
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Print internal information about the specified {\tt ESMF\_VM} to
!   {\tt stdout}.\\
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

    ! Flush before crossing language interface to ensure correct output order
    call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface.
    call c_ESMC_VMPrint(vm, localrc) 
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMRecv - Receive data from srcPet
!
! !INTERFACE:
!  subroutine ESMF_VMRecv(vm, recvData, count, srcPet, &
!    syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                     intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target,  intent(out)           :: recvData(:)  
!    integer,                           intent(in)            :: count
!    integer,                           intent(in)            :: srcPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),              intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),             intent(out), optional :: commhandle
!    integer,                           intent(out), optional :: rc           
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Receive contiguous data from {\tt srcPet} within the same {\tt ESMF\_VM} 
!   object.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}, 
!   {\tt ESMF\_TYPEKIND\_CHARACTER}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[recvData] 
!        Contiguous data array for data to be received.
!   \item[count] 
!        Number of elements to be received.
!   \item[srcPet] 
!        Sending PET.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive 4-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvI4(vm, recvData, count, srcPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)  
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMRecv(vm, recvData(1), size, srcPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMRecvNB(vm, recvData(1), size, srcPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvI8()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvI8(vm, recvData, count, srcPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)  
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 8 ! 8 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMRecv(vm, recvData(1), size, srcPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMRecvNB(vm, recvData(1), size, srcPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvR4()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvR4(vm, recvData, count, srcPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)  
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMRecv(vm, recvData(1), size, srcPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMRecvNB(vm, recvData(1), size, srcPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
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
  subroutine ESMF_VMRecvR8(vm, recvData, count, srcPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)  
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 8 ! 8 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMRecv(vm, recvData(1), size, srcPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMRecvNB(vm, recvData(1), size, srcPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
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
  subroutine ESMF_VMRecvLogical(vm, recvData, count, srcPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_Logical), target, intent(out)           :: recvData(:)  
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMRecv(vm, recvData(1), size, srcPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMRecvNB(vm, recvData(1), size, srcPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvChar()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive Character

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvChar(vm, recvData, count, srcPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*),            intent(out)           :: recvData
    integer,                 intent(in)            :: count
    integer,                 intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 1 ! 1 byte
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMRecv(vm, recvData, size, srcPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMRecvNB(vm, recvData, size, srcPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvChar
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvCharArray()"
!BOPI
! !IROUTINE: ESMF_VMRecv - Receive Character array

! !INTERFACE:
  ! Private name; call using ESMF_VMRecv()
  subroutine ESMF_VMRecvCharArray(vm, recvData, count, srcPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*),            intent(out)           :: recvData(*)
    integer,                 intent(in)            :: count
    integer,                 intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif

    if (count > 0) then
      ! There is data to be received
      size = count * 1 ! 1 byte
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMRecv(vm, recvData, size, srcPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMRecvNB(vm, recvData, size, srcPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMRecvCharArray
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMReduce - Reduce data from across VM
!
! !INTERFACE:
!  subroutine ESMF_VMReduce(vm, sendData, recvData, count, &
!    reduceflag, rootPet, syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: count
!    type(ESMF_Reduce_Flag),           intent(in)            :: reduceflag
!    integer,                          intent(in)            :: rootPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that reduces a contiguous data 
!   array across the {\tt ESMF\_VM} object into a contiguous data array of 
!   the same <type><kind>. The result array is returned on {\tt rootPet}. 
!   Different reduction operations can be specified.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!
!   {\sc Todo:} The current version of this method does not provide an 
!   implementation of the {\em non-blocking} feature. When calling this 
!   method with {\tt syncflag = ESMF\_SYNC\_NONBLOCKING}, error code 
!   {\tt ESMF\_RC\_NOT\_IMPL} will be returned and an error will be 
!   logged.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. All PETs must specify a
!        valid source array.
!   \item[recvData] 
!        Contiguous data array for data to be received. Only the {\tt recvData}
!        array specified by the {\tt rootPet} will be used by this method.
!   \item[count] 
!        Number of elements in sendData and recvData. Must be the same on all
!        PETs.
!   \item[reduceflag] 
!        Reduction operation. See section \ref{const:reduce} for a list of 
!        valid reduce operations.
!   \item[rootPet] 
!        PET on which reduced data is returned.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
    rootPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    type(ESMF_Reduce_Flag),        intent(in)            :: reduceflag
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_I4, reduceflag, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMReduceI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMReduceI8()"
!BOPI
! !IROUTINE: ESMF_VMReduce - Reduce 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMReduce()
  subroutine ESMF_VMReduceI8(vm, sendData, recvData, count, reduceflag, &
    rootPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    type(ESMF_Reduce_Flag),        intent(in)            :: reduceflag
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_I8, reduceflag, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMReduceI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMReduceR4()"
!BOPI
! !IROUTINE: ESMF_VMReduce - Reduce 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMReduce()
  subroutine ESMF_VMReduceR4(vm, sendData, recvData, count, reduceflag, &
    rootPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    type(ESMF_Reduce_Flag),     intent(in)            :: reduceflag
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_R4, reduceflag, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

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
    rootPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    type(ESMF_Reduce_Flag),     intent(in)            :: reduceflag
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    if (count > 0) then
      ! Call into the C++ interface.
      call c_ESMC_VMReduce(vm, sendData(1), recvData(1), count, &
        ESMF_TYPEKIND_R8, reduceflag, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMReduceR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMScatter - Scatter data across VM
!
! !INTERFACE:
!  subroutine ESMF_VMScatter(vm, sendData, recvData, count, &
!    rootPet, syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: count
!    integer,                          intent(in)            :: rootPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that scatters contiguous data 
!   from the {\tt rootPet} to all PETs across the {\tt ESMF\_VM} object
!   (including {\tt rootPet}).
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. Only the {\tt sendData}
!        array specified by the {\tt rootPet} will be used by this method.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid destination array.
!   \item[count] 
!        Number of elements to be sent from {\tt rootPet} to each of the PETs.
!        Must be the same on all PETs.
!   \item[rootPet] 
!        PET that holds data that is being scattered.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
  subroutine ESMF_VMScatterI4(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#define ESMF_METHOD "ESMF_VMScatterI8()"
!BOPI
! !IROUTINE: ESMF_VMScatter - Scatter 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterI8(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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

  end subroutine ESMF_VMScatterI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterR4()"
!BOPI
! !IROUTINE: ESMF_VMScatter - Scatter 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatter()
  subroutine ESMF_VMScatterR4(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMScatterR8(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMScatterLogical(vm, sendData, recvData, count, rootPet, &
    keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_Logical), target, intent(in)            :: sendData(:)
    type(ESMF_Logical), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    size = count * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMScatter(vm, sendData(1), recvData(1), size, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMScatterNB(vm, sendData(1), recvData(1), size, rootPet, &
        localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!  subroutine ESMF_VMScatterV(vm, sendData, sendCounts, &
!    sendOffsets, recvData, recvCount, rootPet, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)
!    integer,                          intent(in)            :: sendCounts(:)
!    integer,                          intent(in)            :: sendOffsets(:)
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)
!    integer,                          intent(in)            :: recvCount
!    integer,                          intent(in)            :: rootPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    integer,                          intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Collective {\tt ESMF\_VM} communication call that scatters contiguous data 
!   from the {\tt rootPet} to all PETs across the {\tt ESMF\_VM} object
!   (including {\tt rootPet}).
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData]
!        Contiguous data array holding data to be sent. Only the {\tt sendData}
!        array specified by the {\tt rootPet} will be used by this method.
!   \item[sendCounts] 
!        Number of {\tt sendData} elements to be sent to corresponding
!        receive PET.
!   \item[sendOffsets] 
!        Offsets in units of elements in {\tt sendData} marking the start of
!        element sequence to be sent to receive PET.
!   \item[recvData] 
!        Contiguous data array for data to be received. All PETs must specify a
!        valid {\tt recvData} argument.
!   \item[recvCount] 
!        Number of {\tt recvData} elements to receive by local PET from
!        {\tt rootPet}.
!   \item[rootPet]
!        PET that holds data that is being scattered.
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
    recvData, recvCount, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCounts(:)
    integer,                       intent(in)            :: sendOffsets(:)
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCount
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMScatterV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCount, ESMF_TYPEKIND_I4, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterVI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterVI8()"
!BOPI
! !IROUTINE: ESMF_VMScatterV - ScatterV 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMScatterV()
  subroutine ESMF_VMScatterVI8(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCount, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCounts(:)
    integer,                       intent(in)            :: sendOffsets(:)
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCount
    integer,                       intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMScatterV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCount, ESMF_TYPEKIND_I8, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterVI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMScatterVR4()"
!BOPI
! !IROUTINE: ESMF_VMScatterV - ScatterV 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMScatterV()
  subroutine ESMF_VMScatterVR4(vm, sendData, sendCounts, sendOffsets, &
    recvData, recvCount, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCounts(:)
    integer,                    intent(in)            :: sendOffsets(:)
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCount
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMScatterV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCount, ESMF_TYPEKIND_R4, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    recvData, recvCount, rootPet, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)
    integer,                    intent(in)            :: sendCounts(:)
    integer,                    intent(in)            :: sendOffsets(:)
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)
    integer,                    intent(in)            :: recvCount
    integer,                    intent(in)            :: rootPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                    intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMScatterV(vm, sendData(1), sendCounts(1), sendOffsets(1), &
      recvData(1), recvCount, ESMF_TYPEKIND_R8, rootPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMScatterVR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMSend - Send data to dstPet
!
! !INTERFACE:
!  subroutine ESMF_VMSend(vm, sendData, count, dstPet, &
!    syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)  
!    integer,                          intent(in)            :: count
!    integer,                          intent(in)            :: dstPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc           
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Send contiguous data to {\tt dstPet} within the same {\tt ESMF\_VM} object.
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8}, 
!   {\tt ESMF\_TYPEKIND\_LOGICAL}, 
!   {\tt ESMF\_TYPEKIND\_CHARACTER}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contiguous data array holding data to be sent.
!   \item[count] 
!        Number of elements to be sent.
!   \item[dstPet] 
!        Receiving PET.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
  subroutine ESMF_VMSendI4(vm, sendData, count, dstPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)  
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: dstPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMSend(vm, sendData(1), size, dstPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMSendNB(vm, sendData(1), size, dstPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendI8()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendI8(vm, sendData, count, dstPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)  
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: dstPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 8 ! 8 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMSend(vm, sendData(1), size, dstPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMSendNB(vm, sendData(1), size, dstPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendR4()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send 4-byte reals

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendR4(vm, sendData, count, dstPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)  
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: dstPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMSend(vm, sendData(1), size, dstPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMSendNB(vm, sendData(1), size, dstPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
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
  subroutine ESMF_VMSendR8(vm, sendData, count, dstPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)  
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: dstPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 8 ! 8 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMSend(vm, sendData(1), size, dstPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMSendNB(vm, sendData(1), size, dstPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
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
  subroutine ESMF_VMSendLogical(vm, sendData, count, dstPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_Logical), target, intent(in)            :: sendData(:)  
    integer,                    intent(in)            :: count
    integer,                    intent(in)            :: dstPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count * 4 ! 4 bytes
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMSend(vm, sendData(1), size, dstPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMSendNB(vm, sendData(1), size, dstPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendChar()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send Character

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendChar(vm, sendData, count, dstPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*),            intent(in)            :: sendData
    integer,                 intent(in)            :: count
    integer,                 intent(in)            :: dstPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    if (count > 0) then
      ! There is data to be received
      size = count ! 1 byte
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMSend(vm, sendData, size, dstPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMSendNB(vm, sendData, size, dstPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendChar
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendCharArray()"
!BOPI
! !IROUTINE: ESMF_VMSend - Send Character

! !INTERFACE:
  ! Private name; call using ESMF_VMSend()
  subroutine ESMF_VMSendCharArray(vm, sendData, count, dstPet, keywordEnforcer, &
    syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*),            intent(in)            :: sendData(*)
    integer,                 intent(in)            :: count
    integer,                 intent(in)            :: dstPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif

    if (count > 0) then
      ! There is data to be received
      size = count ! 1 byte
      ! Call into the C++ interface.
      if (blocking) then
        call c_ESMC_VMSend(vm, sendData, size, dstPet, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_VMSendNB(vm, sendData, size, dstPet, localcommhandle, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! Check if we need to pass back the commhandle
        if (present(commhandle)) then
          commhandle = localcommhandle  ! copy the commhandle pointer back
          ! Set init code
          ESMF_INIT_SET_CREATED(commhandle)
        endif
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMSendCharArray
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_VMSendRecv - Send and Recv data to and from PETs
!
! !INTERFACE:
!  subroutine ESMF_VMSendRecv(vm, sendData, sendCount, dstPet, &
!    recvData, recvCount, srcPet, syncflag, commhandle, rc)
!
! !ARGUMENTS:
!    type(ESMF_VM),                    intent(in)            :: vm
!    <type>(ESMF_KIND_<kind>), target, intent(in)            :: sendData(:)  
!    integer,                          intent(in)            :: sendCount
!    integer,                          intent(in)            :: dstPet
!    <type>(ESMF_KIND_<kind>), target, intent(out)           :: recvData(:)  
!    integer,                          intent(in)            :: recvCount
!    integer,                          intent(in)            :: srcPet
!type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!    type(ESMF_Sync_Flag),             intent(in),  optional :: syncflag
!    type(ESMF_CommHandle),            intent(out), optional :: commhandle
!    integer,                          intent(out), optional :: rc           
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Send contiguous data to {\tt dstPet} within the same {\tt ESMF\_VM} object
!   while receiving contiguous data from {\tt srcPet} within the same 
!   {\tt ESMF\_VM} object. The {\tt sendData} and {\tt recvData} arrays must be
!   disjoint!
!
!   This method is overloaded for:
!   {\tt ESMF\_TYPEKIND\_I4}, {\tt ESMF\_TYPEKIND\_I8},
!   {\tt ESMF\_TYPEKIND\_R4}, {\tt ESMF\_TYPEKIND\_R8},
!   {\tt ESMF\_TYPEKIND\_LOGICAL}, 
!   {\tt ESMF\_TYPEKIND\_CHARACTER}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[sendData] 
!        Contiguous data array holding data to be sent.
!   \item[sendCount] 
!        Number of elements to be sent.
!   \item[dstPet] 
!        PET that holds {\tt recvData}.
!   \item[recvData] 
!        Contiguous data array for data to be received.
!   \item[recvCount] 
!        Number of elements to be received.
!   \item[srcPet] 
!        PET that holds {\tt sendData}.
!   \item[{[syncflag]}]
!        Flag indicating whether this call behaves blocking or non-blocking.
!        The default is {\tt ESMF\_SYNC\_BLOCKING}. See section
!        \ref{const:sync} for a complete list of options.
!   \item[{[commhandle]}]
!        If present, a communication handle will be returned in case of a 
!        non-blocking request (see argument {\tt syncflag}). The
!        {\tt commhandle} can be used in {\tt ESMF\_VMCommWait()} to block the
!        calling PET until the communication call has finished PET-locally. If
!        no {\tt commhandle} was supplied to a non-blocking call the VM method
!        {\tt ESMF\_VMCommWaitAll()} may be used to block on all currently queued
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
  subroutine ESMF_VMSendRecvI4(vm, sendData, sendCount, dstPet, &
    recvData, recvCount, srcPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I4), target, intent(in)            :: sendData(:)  
    integer,                       intent(in)            :: sendCount
    integer,                       intent(in)            :: dstPet
    integer(ESMF_KIND_I4), target, intent(out)           :: recvData(:)  
    integer,                       intent(in)            :: recvCount
    integer,                       intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#define ESMF_METHOD "ESMF_VMSendRecvI8()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv 8-byte integers

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvI8(vm, sendData, sendCount, dstPet, &
    recvData, recvCount, srcPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    integer(ESMF_KIND_I8), target, intent(in)            :: sendData(:)  
    integer,                       intent(in)            :: sendCount
    integer,                       intent(in)            :: dstPet
    integer(ESMF_KIND_I8), target, intent(out)           :: recvData(:)  
    integer,                       intent(in)            :: recvCount
    integer,                       intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 8 ! 8 bytes
    recvSize = recvCount * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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

  end subroutine ESMF_VMSendRecvI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMSendRecvR4()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv 4-byte real

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvR4(vm, sendData, sendCount, dstPet, &
    recvData, recvCount, srcPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R4), target, intent(in)            :: sendData(:)  
    integer,                    intent(in)            :: sendCount
    integer,                    intent(in)            :: dstPet
    real(ESMF_KIND_R4), target, intent(out)           :: recvData(:)  
    integer,                    intent(in)            :: recvCount
    integer,                    intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMSendRecvR8(vm, sendData, sendCount, dstPet, &
    recvData, recvCount, srcPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    real(ESMF_KIND_R8), target, intent(in)            :: sendData(:)  
    integer,                    intent(in)            :: sendCount
    integer,                    intent(in)            :: dstPet
    real(ESMF_KIND_R8), target, intent(out)           :: recvData(:)  
    integer,                    intent(in)            :: recvCount
    integer,                    intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 8 ! 8 bytes
    recvSize = recvCount * 8 ! 8 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMSendRecvLogical(vm, sendData, sendCount, dstPet, &
    recvData, recvCount, srcPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),              intent(in)            :: vm
    type(ESMF_Logical), target, intent(in)            :: sendData(:)  
    integer,                    intent(in)            :: sendCount
    integer,                    intent(in)            :: dstPet
    type(ESMF_Logical), target, intent(out)           :: recvData(:)  
    integer,                    intent(in)            :: recvCount
    integer,                    intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),       intent(in),  optional :: syncflag
    type(ESMF_CommHandle),      intent(out), optional :: commhandle
    integer,                    intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount * 4 ! 4 bytes
    recvSize = recvCount * 4 ! 4 bytes
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData(1), sendSize, dstPet, &
        recvData(1), recvSize, srcPet, localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
#define ESMF_METHOD "ESMF_VMSendRecvChar()"
!BOPI
! !IROUTINE: ESMF_VMSendRecv - SendRecv Character

! !INTERFACE:
  ! Private name; call using ESMF_VMSendRecv()
  subroutine ESMF_VMSendRecvChar(vm, sendData, sendCount, dstPet, &
    recvData, recvCount, srcPet, keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),           intent(in)            :: vm
    character(*),            intent(in)            :: sendData
    integer,                 intent(in)            :: sendCount
    integer,                 intent(in)            :: dstPet
    character(*),            intent(out)           :: recvData
    integer,                 intent(in)            :: recvCount
    integer,                 intent(in)            :: srcPet
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),    intent(in),  optional :: syncflag
    type(ESMF_CommHandle),   intent(out), optional :: commhandle
    integer,                 intent(out), optional :: rc           
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

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    sendSize = sendCount ! 1 byte
    recvSize = recvCount ! 1 byte
    ! Call into the C++ interface.
    if (blocking) then
      call c_ESMC_VMSendRecv(vm, sendData, sendSize, dstPet, &
        recvData, recvSize, srcPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call c_ESMC_VMSendRecvNB(vm, sendData, sendSize, dstPet, &
        recvData, recvSize, srcPet, localcommhandle, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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

  end subroutine ESMF_VMSendRecvChar
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMThreadBarrier()"
!BOPI
! !IROUTINE: ESMF_VMThreadBarrier - PET thread group wide barrier

! !INTERFACE:
  subroutine ESMF_VMThreadBarrier(vm, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,       intent(out), optional :: rc           
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
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Call into the C++ interface.
    call c_ESMC_VMThreadBarrier(vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMValidate(vm, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)            :: vm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,       intent(out), optional :: rc  
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
    
    ! Call into the C++ interface.
    call c_ESMC_VMValidate(vm, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMWtime()"
!BOP
! !IROUTINE: ESMF_VMWtime - Get floating-point number of seconds

! !INTERFACE:
  subroutine ESMF_VMWtime(time, keywordEnforcer, rc)
!
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(out)           :: time
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Get floating-point number of seconds of elapsed wall-clock time since the
!   beginning of execution of the application.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  recursive subroutine ESMF_VMWtimeDelay(delay, keywordEnforcer, rc)
!
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(in)            :: delay
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Delay execution for amount of seconds.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMWtimePrec(prec, keywordEnforcer, rc)
!
! !ARGUMENTS:
    real(ESMF_KIND_R8), intent(out)           :: prec
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Get a run-time estimate of the timer precision as floating-point number 
!   of seconds. This is a relatively expensive call since the timer precision
!   is measured several times before the maximum is returned as the estimate.
!   The returned value is PET-specific and may differ across the VM 
!   context.
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
!   Initialize the Global VM.
!
!   The arguments are:
!   \begin{description}
!   \item[{[mpiCommunicator]}] 
!        MPI communicator defining the group of processes on which the
!        ESMF application is running.
!        If not sepcified, defaults to {\tt MPI\_COMM\_WORLD}
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

    ! Call into the C++ interface.
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
!   Finalize Global VM.
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

    ! Call into the C++ interface.
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
!   Abort Global VM.
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

    ! Call into the C++ interface.
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
  recursive subroutine ESMF_VMShutdown(vm, vmplan, vm_info, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)              :: vm
    type(ESMF_VMPlan),  intent(in)              :: vmplan
    type(ESMF_Pointer), intent(inout)           :: vm_info
    integer,            intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Shutdown a VM.
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

    ! Call into the C++ interface.
    call c_ESMC_VMShutdown(vm, vmplan, vm_info, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  recursive function ESMF_VMGetInit(vm) 
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
  recursive subroutine ESMF_VMSetInitCreated(vm, rc)
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
  recursive subroutine ESMF_VMGetThis(vm, this, rc)
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
  recursive subroutine ESMF_VMSetThis(vm, this, rc)
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
    type(ESMF_VMPlan),       intent(inout)         :: vmplan
    type(ESMF_VM),           intent(in)            :: vm
    integer,                 intent(in)            :: npetlist
    integer,                 intent(in)            :: petlist(:)
    type(ESMF_Context_Flag), intent(in)            :: contextflag
    integer,                 intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Construct a default plan.
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
    
    ! Call into the C++ interface.
    call c_ESMC_VMPlanConstruct(vmplan, vm, npetlist, petlist, contextflag, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!   Destruct a vmplan.
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
    
    ! Call into the C++ interface.
    call c_ESMC_VMPlanDestruct(vmplan, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!   Set up a MaxPEs vmplan.
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

    ! Call into the C++ interface.
    call c_ESMC_VMPlanMaxPEs(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!   Set up a MaxThreads vmplan.
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

    ! Call into the C++ interface.
    call c_ESMC_VMPlanMaxThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!   Set up a MinThreads vmplan.
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

    ! Call into the C++ interface.
    call c_ESMC_VMPlanMinThreads(vmplan, vm, max, &
      pref_intra_process, pref_intra_ssi, pref_inter_ssi, &
      npetlist, petlist, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    logical :: ESMF_VMIdCompare
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(in)            :: vmId1
    type(ESMF_VMId),   intent(in)            :: vmId2
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Compare two ESMF_VMId objects.
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
    type(ESMF_Logical)      :: tf

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    call c_ESMC_VMIdCompare(vmId1, vmId2, tf, localrc)
    ESMF_VMIdCompare = tf == ESMF_TRUE
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_VMIdCompare
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdCopy()"
!BOPI
! !IROUTINE: ESMF_VMIdCopy - Copy contents of ESMF_VMId objects

! !INTERFACE:
  subroutine ESMF_VMIdCopy(dest, source, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(inout)         :: dest(:)
    type(ESMF_VMId),   intent(in)            :: source(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Copy the contents of ESMF_VMId objects.  Note that the destination
!   objects must have been (deeply) allocated prior to calling this
!   copy.
!
!   The arguments are:
!   \begin{description}
!   \item[dest]
!        Destination ESMF_VMId object array
!   \item[source]
!        Source ESMF_VMId object array
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: i
    type(ESMF_Logical)      :: tf

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (size (dest) /= size (source)) then
      localrc = ESMF_RC_ARG_SIZE
      if (ESMF_LogFoundError(localrc, msg='size (dest) /= size (source)', &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end if

    ! Call into the C++ interface
    do, i=1, size (source)
      call c_ESMC_VMIdCopy(dest(i), source(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end do

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdCopy
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdPrint_s()"
!BOPI
! !IROUTINE: ESMF_VMIdPrint - Print an ESMF_VMId object

! !INTERFACE:
  subroutine ESMF_VMIdPrint_s(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(in)            :: vmId
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Print an ESMF_VMId object.
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

    ! Flush before crossing language interface to ensure correct output order
    call ESMF_UtilIOUnitFlush (ESMF_UtilIOstdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface
    call c_ESMC_VMIdPrint(vmId, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdPrint_s
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdPrint_v()"
!BOPI
! !IROUTINE: ESMF_VMIdPrint - Print an ESMF_VMId object

! !INTERFACE:
  subroutine ESMF_VMIdPrint_v(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(in)            :: vmId(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Print an ESMF_VMId object.
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
    integer                 :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    print *, 'ESMF_VMIdPrint: Fortran this pointer addresses:'
    print '(i0,a,z16)', (i, ': z', vmId(i)%this, i=1, size (vmId))

    ! Flush before crossing language interface to ensure correct output order
    call ESMF_UtilIOUnitFlush (ESMF_UtilIOstdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface
    call c_ESMC_VMIdPrint(vmId, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdPrint_v
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdCreate()"
!BOPI
! !IROUTINE: ESMF_VMIdCreate - Create an ESMF_VMId object

! !INTERFACE:
  subroutine ESMF_VMIdCreate_s(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(inout)         :: vmId
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Create an ESMF_VMId object. This allocates memory on the C side.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdCreate_s
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdCreate()"
!BOPI
! !IROUTINE: ESMF_VMIdCreate - Create an array of ESMF_VMId objects

! !INTERFACE:
  subroutine ESMF_VMIdCreate_v(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(inout)         :: vmId(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Create an ESMF_VMId object. This allocates memory on the C side.
!
!   The arguments are:
!   \begin{description}
!   \item[vmId] 
!        Array of ESMF_VMId objects
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    do, i=1, size (vmId)
      call c_ESMC_VMIdCreate(vmId(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end do

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdCreate_v
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdDestroy()"
!BOPI
! !IROUTINE: ESMF_VMIdDestroy - Destroy an ESMF_VMId object

! !INTERFACE:
  subroutine ESMF_VMIdDestroy_s(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(inout)         :: vmId
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Destroy an ESMF_VMId object. This frees memory on the C side.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdDestroy_s
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMIdDestroy()"
!BOPI
! !IROUTINE: ESMF_VMIdDestroy - Destroy an array of ESMF_VMId objects

! !INTERFACE:
  subroutine ESMF_VMIdDestroy_v(vmId, rc)
!
! !ARGUMENTS:
    type(ESMF_VMId),   intent(inout)         :: vmId(:)
    integer,           intent(out), optional :: rc           
!
! !DESCRIPTION:
!   Destroy an ESMF_VMId object. This frees memory on the C side.
!
!   The arguments are:
!   \begin{description}
!   \item[vmId] 
!        Array of ESMF_VMId objects
!   \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: i

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface
    do, i=1, size (vmId)
      call c_ESMC_VMIdDestroy(vmId(i), localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end do

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMIdDestroy_v
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMAllToAllV()"
!BOPI
! !IROUTINE: ESMF_VMAllToAllV - AllToAllV VMId types

! !INTERFACE:
  ! Private name; call using ESMF_VMAllToAllV()
  subroutine ESMF_VMAllToAllVVMId(vm,  &
      sendData, sendCounts, sendOffsets, &
      recvData, recvCounts, recvOffsets, &
      keywordEnforcer, syncflag, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),                 intent(in)            :: vm
    type(ESMF_VMId), target,       intent(in)            :: sendData(:)
    integer,                       intent(in)            :: sendCounts(:)
    integer,                       intent(in)            :: sendOffsets(:)
    type(ESMF_VMId), target,       intent(out)           :: recvData(:)
    integer,                       intent(in)            :: recvCounts(:)
    integer,                       intent(in)            :: recvOffsets(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_Sync_Flag),          intent(in),  optional :: syncflag
    type(ESMF_CommHandle),         intent(out), optional :: commhandle
    integer,                       intent(out), optional :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Not implemented features
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="- non-blocking mode not yet implemented", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! Call into the C++ interface.
    localrc = ESMF_RC_INTNRL_BAD
!    call c_ESMC_VMAllToAllVVMId(vm,  &
!        sendData, sendCounts, sendOffsets, &
!        recvData, recvCounts, recvOffsets, &
!        localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMAllToAllVVMId
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMBcastVMId()"
!BOPI
! !IROUTINE: ESMF_VMBcastVMId - Broadcast ESMF_VMId array

! !INTERFACE:
  subroutine ESMF_VMBcastVMId(vm, bcstData, count, rootPet, syncflag, &
    commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_VMId), target,  intent(inout)           :: bcstData(:)
    integer,                  intent(in)              :: count
    integer,                  intent(in)              :: rootPet
    type(ESMF_Sync_Flag),  intent(in),   optional  :: syncflag
    type(ESMF_CommHandle),    intent(out),  optional  :: commhandle
    integer,                  intent(out),  optional  :: rc
!         
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: mypet, npets
    integer                 :: i, k
    logical                 :: blocking

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    ! Initialize commhandle to an invalid pointer
    if (present(commhandle)) commhandle%this = ESMF_NULL_POINTER

    ! Decide whether this is blocking or non-blocking
    blocking = .true. !default is blocking
    if (present(syncflag)) then
      if (syncflag == ESMF_SYNC_NONBLOCKING) blocking = .false. ! non-blocking
    endif
    
    ! Not implemented features
    if (.not. blocking) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="- non-blocking mode not yet implemented", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (count > 0) then
      call c_ESMC_VMBcastVMId (vm, bcstData, count, rootPet, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_VMBcastVMId
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VMRecvVMId()"
!BOPI
! !IROUTINE: ESMF_VMRecvVMId - Receive VMId

! !INTERFACE:
  subroutine ESMF_VMRecvVMId(vm, vmID, srcPet, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_VMId),          intent(out)             :: vmId
    integer,                  intent(in)              :: srcPet
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive {\tt ESMF\_VMId}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[vmId] 
!        {\tt ESMF\_VMId} to be received.
!   \item[srcPet] 
!        Sending PET.
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

    ! Call into the C++ interface.
    call c_ESMC_VMRecvVMId(vm, vmId, srcPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  subroutine ESMF_VMSendVMId(vm, vmID, dstPet, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),            intent(in)              :: vm
    type(ESMF_VMId),          intent(in)              :: vmId
    integer,                  intent(in)              :: dstPet
    integer,                  intent(out),  optional  :: rc           
!
! !DESCRIPTION:
!   Receive {\tt ESMF\_VMId}.
!
!   The arguments are:
!   \begin{description}
!   \item[vm] 
!        {\tt ESMF\_VM} object.
!   \item[vmId] 
!        {\tt ESMF\_VMId} to be sent.
!   \item[dstPet] 
!        Receiving PET.
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

    ! Call into the C++ interface.
    call c_ESMC_VMSendVMId(vm, vmId, dstPet, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    
    ! Call into the C++ interface.
    !todo: call c_ESMC_CommHandleValidate(commhandle, localrc)
    localrc = ESMF_SUCCESS  ! remove when todo is done.
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
  
  implicit none
  
  integer, intent(out), optional  :: localPet
  integer, intent(out), optional  :: petCount
    
  call ESMF_VMGet(GlobalVM, localPet=localPet, petCount=petCount)
  
end subroutine f_ESMF_VMGlobalGet


subroutine f_ESMF_VMAbort()
  use ESMF_VMMod
  
  implicit none
    
  call ESMF_VMAbort()
  
end subroutine f_ESMF_VMAbort

