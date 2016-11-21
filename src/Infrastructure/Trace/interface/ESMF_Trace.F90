! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
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

  implicit none
  private

  public ESMF_TraceOpen
  public ESMF_TraceClose
  public ESMF_TraceEventPhaseEnter
  public ESMF_TraceEventPhaseExit

contains

  subroutine ESMF_TraceOpen(bufSize, traceDir, streamId, rc)
    integer,          intent(in)             :: bufSize
    character(len=*), intent(in)             :: traceDir
    integer,          intent(in)             :: streamId
    integer,          intent(out), optional  :: rc
    call c_esmftrace_open(bufSize, traceDir, streamId, rc)
  end subroutine ESMF_TraceOpen
  
  subroutine ESMF_TraceClose(rc)
    integer,          intent(out), optional :: rc    
    call c_esmftrace_close(rc)
  end subroutine ESMF_TraceClose

  subroutine ESMF_TraceEventPhaseEnter(vmid, baseid, method, phase, rc)
    integer, intent(in)            :: vmid
    integer, intent(in)            :: baseid
    integer, intent(in)            :: method
    integer, intent(in)            :: phase
    integer, intent(out), optional :: rc
    call c_esmftrace_phase_enter(vmid, baseid, method, phase, rc)
  end subroutine ESMF_TraceEventPhaseEnter

  subroutine ESMF_TraceEventPhaseExit(vmid, baseid, method, phase, rc)
    integer, intent(in)            :: vmid
    integer, intent(in)            :: baseid
    integer, intent(in)            :: method
    integer, intent(in)            :: phase
    integer, intent(out), optional :: rc
    call c_esmftrace_phase_exit(vmid, baseid, method, phase, rc)
  end subroutine ESMF_TraceEventPhaseExit

end module
