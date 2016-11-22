// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Trace include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_TRACE_H
#define ESMCI_TRACE_H

/*
namespace ESMCI {

  class Trace {
  
  private:
    //int stream_id;           //id for this stream, usually localPet
    //char stream_path[1024];  //path to output file
    //uint8_t *buffer;         //trace event data buffer
    
  
  public:
    int Open(int stream_id, 
	     const char trace_dir[],
	     int buffer_size);
    int Close();
    
  }

}
*/

namespace ESMCI { 
  int TraceOpen(unsigned int buf_size, const char *trace_dir, 
		int stream_id);
  void TraceClose();

  ////////////////////////////////

  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, 
			    int *ep_method, int *ep_phase);
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, 
			   int *ep_method, int *ep_phase);
  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid, 
			    int *ep_method, int *ep_phase);
  void TraceEventPhasePrologueExit(int *ep_vmid, int *ep_baseid, 
                                   int *ep_method, int *ep_phase);
  void TraceEventPhaseEpilogueEnter(int *ep_vmid, int *ep_baseid, 
                                    int *ep_method, int *ep_phase);
  void TraceEventPhaseEpilogueExit(int *ep_vmid, int *ep_baseid, 
                                   int *ep_method, int *ep_phase);

  void TraceEventComponentInfo(int *ep_vmid, int *ep_baseid,
                               const char *ep_name);

}

#endif
