#include <string.h>
#include <stdlib.h>
#include <stdio.h>

//#include <sstream>
//#include <cstring>
//#include <fstream>
//#include <cstdlib>
//#include <vector>
//#include <algorithm>

#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_Trace.h"

using std::string;


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

  void FTN_X(c_esmftrace_phase_prologue_enter)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhasePrologueEnter(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmftrace_phase_prologue_exit)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhasePrologueExit(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmftrace_phase_epilogue_enter)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhaseEpilogueEnter(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmftrace_phase_epilogue_exit)(int *vmid, int *baseid, int *method, int *phase, int *rc)
  {
    ESMCI::TraceEventPhaseEpilogueExit(vmid, baseid, method, phase);
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }

  void FTN_X(c_esmftrace_component_info)(int *vmid, int *baseid, const char *name, int *rc, 
                                         ESMCI_FortranStrLenArg nlen)
  {
    string cname = string(name, ESMC_F90lentrim (name, nlen));
    ESMCI::TraceEventComponentInfo(vmid, baseid, cname.c_str());
    if (rc != NULL) *rc = ESMF_SUCCESS;
  }
    

} // extern "C"
