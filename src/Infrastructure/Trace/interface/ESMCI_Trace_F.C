#include <string.h>
#include <stdlib.h>

#include "ESMCI_Macros.h"
#include "ESMCI_Trace.h"

// C interface called from Fortran

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

extern "C" {

  void FTN_X(c_esmftrace_open)
     (				   
      int *buf_size,  
      const char *trace_dir,           
      int *stream_id,
      int *rc,                        
      ESMCI_FortranStrLenArg nlen)  //strlen for trace_dir 
  {      
    int localrc = ESMCI::TraceOpen((unsigned int) *buf_size, trace_dir, *stream_id);
    if (rc != NULL) *rc = localrc;
  } 

  void FTN_X(c_esmftrace_close)(int *rc) 
  {
    ESMCI::TraceClose();
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }
  
  void FTN_X(c_esmftrace_phase_enter)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhaseEnter(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmftrace_phase_exit)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhaseExit(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }
    

} // extern "C"
