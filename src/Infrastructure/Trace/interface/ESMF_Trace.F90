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
#define ESMF_FILENAME "ESMF_Trace.F90"
!==============================================================================
!
!     ESMF Trace module
module ESMF_TraceMod

#include "ESMF.h"

!BOPI
! !MODULE: ESMF_TraceMod - Tracing module
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to
! ESMF's tracing capability.  
!
!
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod 
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_TraceRegionEnter
  public ESMF_TraceRegionExit

! - ESMF-internal methods:
  public ESMF_TraceOpen
  public ESMF_TraceClose
  public ESMF_TraceMemInfo

  ! - ESMF-internal - only for unit tests
  public ESMF_TraceTest_GetMPIWaitStats
  public ESMF_TraceTest_CheckMPIRegion
  !EOPI
  
contains

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceOpen()"
!BOPI 
! !IROUTINE: ESMF_TraceOpen - Initialize tracing infrastructure
! 
! !INTERFACE: 
  subroutine ESMF_TraceOpen(traceDir, profileToLog, rc)
! !ARGUMENTS: 
    character(len=*), intent(in)             :: traceDir
    integer,          intent(in)             :: profileToLog
    integer,          intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Initialize tracing infrastructure including creating output directory
!   and opening files for trace output.
!
!EOPI
!-------------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS
    
    call c_esmftrace_open(traceDir, profileToLog, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceOpen
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceClose()"
!BOPI 
! !IROUTINE: ESMF_TraceClose - Finalize tracing infrastructure
! 
! !INTERFACE: 
  subroutine ESMF_TraceClose(rc)
! !ARGUMENTS:
    integer,          intent(out), optional :: rc    
!
! !DESCRIPTION:
!   Close the tracing infrastructure, flushing any
!   outstanding events to disk.  Once closed, a trace
!   cannot be re-opened.
!
!EOPI
!-------------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS

    call c_esmftrace_close(rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
  end subroutine ESMF_TraceClose

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceRegionEnter()"
!BOP 
! !IROUTINE: ESMF_TraceRegionEnter - Trace user-defined region entry event
! 
! !INTERFACE: 
  subroutine ESMF_TraceRegionEnter(name, rc)
! !ARGUMENTS: 
    character(len=*), intent(in) :: name
    integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Record an event in the trace for this PET indicating entry
!   into a user-defined region with the given name.  This call
!   must be paired with a call to {\tt ESMF\_TraceRegionExit()}
!   with a matching {\tt name} parameter.  User-defined regions may be
!   nested.
!   If tracing is disabled on the calling PET or for the application
!   as a whole, no event will be recorded and
!   the call will return immediately.
!
! The arguments are:
! \begin{description}
! \item[{name}]
!   A user-defined name for the region of code being entered
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}    
!EOP
!-------------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS 
    
    call c_esmftrace_region_enter(name, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceRegionEnter
  
  
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceRegionExit()"
!BOP 
! !IROUTINE: ESMF_TraceRegionExit - Trace user-defined region exit event
! 
! !INTERFACE: 
  subroutine ESMF_TraceRegionExit(name, rc)
! !ARGUMENTS: 
    character(len=*), intent(in) :: name
    integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
!   Record an event in the trace for this PET indicating exit
!   from a user-defined region with the given name.  This call
!   must appear after a call to {\tt ESMF\_TraceRegionEnter()}
!   with a matching {\tt name} parameter.
!   If tracing is disabled on the calling PET or for the application
!   as a whole, no event will be recorded and
!   the call will return immediately.
!
! The arguments are:
! \begin{description}
! \item[{name}]
!   A user-defined name for the region of code being exited
! \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}    
!EOP
!-------------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS 
    
    call c_esmftrace_region_exit(name, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_TraceRegionExit

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceMemInfo()"
!BOPI 
! !IROUTINE: ESMF_TraceMemInfo - Trace memory usage info
! 
! !INTERFACE: 
  subroutine ESMF_TraceMemInfo(rc)
! !ARGUMENTS: 
    integer, intent(out), optional  :: rc
!
!EOPI
!-------------------------------------------------------------------------------
    if (present(rc)) rc = ESMF_SUCCESS 
    
    call c_esmftrace_mem_info(rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
  end subroutine ESMF_TraceMemInfo
  

#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceTest_GetMPIWaitStats"
!BOPI 
! !IROUTINE: ESMF_TraceTest_GetMPIWaitStats - MPI Statistics - for testing only
! 
! !INTERFACE: 
  subroutine ESMF_TraceTest_GetMPIWaitStats(count, time, rc)
! !ARGUMENTS: 
    integer, intent(out)               :: count
    integer(ESMF_KIND_I8), intent(out) :: time
    integer, intent(out), optional     :: rc
!
!EOPI
!-------------------------------------------------------------------------------

    if (present(rc)) rc = ESMF_SUCCESS 
    call c_esmftracetest_getmpiwaitstats(count, time)
    
  end subroutine ESMF_TraceTest_GetMPIWaitStats

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_TraceTest_CheckMPIRegion"
!BOPI 
! !IROUTINE: ESMF_TraceTest_CheckMPIRegion - MPI Statistics - for testing only
! 
! !INTERFACE: 
  subroutine ESMF_TraceTest_CheckMPIRegion(name, exists, rc)
! !ARGUMENTS: 
    character(len=*), intent(in)       :: name
    integer, intent(out)               :: exists
    integer, intent(out), optional     :: rc
!
!EOPI
!-------------------------------------------------------------------------------

    if (present(rc)) rc = ESMF_SUCCESS 
    call c_esmftracetest_checkmpiregion(name, exists)
    
  end subroutine ESMF_TraceTest_CheckMPIRegion
  
end module
