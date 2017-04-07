// $Id$
/*
 * Writes trace events to the file system.
 *
 * Earth System Modeling Framework
 * Copyright 2002-2016, University Corporation for Atmospheric Research, 
 * Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
 * Laboratory, University of Michigan, National Centers for Environmental 
 * Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
 * NASA Goddard Space Flight Center.
 * Licensed under the University of Illinois-NCSA License.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <babeltrace/ctf-writer/writer.h>
#include <babeltrace/ctf-writer/clock.h>
#include <babeltrace/ctf-writer/stream.h>
#include <babeltrace/ctf-writer/event.h>
#include <babeltrace/ctf-writer/event-types.h>
#include <babeltrace/ctf-writer/event-fields.h>

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_Trace.h"
#include "ESMCI_Comp.h"

#define BT_CHK(_value, _ctx) \
  if ((_value) != 0) {							\
    ESMC_LogDefault.MsgFoundError(ESMC_RC_SYS, "Internal tracing error", _ctx, rc); \
    return;}

#define BT_CHK_NULL(_value, _ctx) \
  if ((_value) == NULL) {						\
    ESMC_LogDefault.MsgFoundError(ESMC_RC_SYS, "Internal tracing error", _ctx, rc); \
    return;}

using std::string;
using std::vector;

namespace ESMCI {
  
  //global trace writer
  struct bt_ctf_writer *bt_writer;
  struct bt_ctf_stream_class *bt_stream_class;
  struct bt_ctf_stream *bt_stream;
  struct bt_ctf_clock *bt_clock;
  
  //field data types
  struct bt_ctf_field_type *bt_type_uint4;
  struct bt_ctf_field_type *bt_type_uint8;
  struct bt_ctf_field_type *bt_type_uint16;
  struct bt_ctf_field_type *bt_type_uint32;
  struct bt_ctf_field_type *bt_type_string;
  struct bt_ctf_field_type *bt_type_enum_control;
  struct bt_ctf_field_type *bt_type_enum_method;
  struct bt_ctf_field_type *bt_type_seq_attr;
  
  //event classes
  struct bt_ctf_event_class *bt_event_class_control;
  struct bt_ctf_event_class *bt_event_class_comp;
  
  static uint64_t get_clock() {
    struct timespec ts;
    
    //clock_gettime(CLOCK_MONOTONIC, &ts);
    clock_gettime(CLOCK_REALTIME, &ts);
    
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
  }

  static vector<string> split(const string& s, const string& delim, const bool keep_empty = true) {
    vector<string> result;
    if (delim.empty()) {
      result.push_back(s);
      return result;
    }
    string::const_iterator substart = s.begin(), subend;
    while (true) {
      subend = search(substart, s.end(), delim.begin(), delim.end());
      string temp(substart, subend);
      if (keep_empty || !temp.empty()) {
	result.push_back(temp);
      }
      if (subend == s.end()) {
	break;
      }
      substart = subend + delim.size();
    }
    return result;
  }
  

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceOpen()"  
  void TraceOpen(const char *trace_dir, int stream_id, int *rc) {
    
    int localrc;
    //char stream_path[ESMC_MAXPATHLEN];
    char stream_dir_root[ESMC_MAXPATHLEN];
    char stream_dir[ESMC_MAXPATHLEN];
        
    if (rc != NULL) *rc = ESMF_SUCCESS;

    ESMC_LogDefault.Write("Enabling ESMF Tracing", ESMC_LOGMSG_INFO);
        
    //make relative path absolute if needed
    if (trace_dir[0] != '/') {
      char cwd[1024];
      if (getcwd(cwd, sizeof(cwd)) == NULL) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_SYS, "Error getting working directory", 
                                      ESMC_CONTEXT, rc);
        return; 
      }
      sprintf(stream_dir_root, "%s/%s", cwd, trace_dir);
      //printf("absolute dir = |%s|\n", stream_dir);
    }
    else {
      sprintf(stream_dir_root, "%s", trace_dir);
    }
    
    struct stat st = {0};
    if (stream_id == 0) {
      if (stat(stream_dir_root, &st) == -1) {
	if (mkdir(stream_dir_root, 0700) == -1) {
	  //printf("mkdir == -1\n");
	  //perror("mkdir()");
	  ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error creating trace root directory", 
					ESMC_CONTEXT, rc);
	  return;
	}
      }
    }
    
    //all PETs wait for directory to be created
    VM *globalvm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
        ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
      return;
    globalvm->barrier();

    //my specific directory
    sprintf(stream_dir, "%s/PET%d", stream_dir_root, stream_id);
    
    if (stat(stream_dir, &st) == -1) {
      if (mkdir(stream_dir, 0700) == -1) {
	//printf("mkdir == -1\n");
	//perror("mkdir()");
	ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error creating trace PET directory", 
				      ESMC_CONTEXT, rc);
	return;
      }
    }
        
    bt_writer = bt_ctf_writer_create(stream_dir);
    BT_CHK_NULL(bt_writer, ESMC_CONTEXT);

    //struct bt_ctf_trace *bt_trace = bt_ctf_writer_get_trace(bt_writer);
    //BT_CHK_NULL(bt_trace, ESMC_CONTEXT);
    
    bt_clock = bt_ctf_clock_create("sys_clock");
    BT_CHK_NULL(bt_clock, ESMC_CONTEXT);
    
    BT_CHK(bt_ctf_writer_add_clock(bt_writer, bt_clock), ESMC_CONTEXT);

    TraceSetupTypes(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
				      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
      return;
    
    bt_stream = bt_ctf_writer_create_stream(bt_writer, bt_stream_class);
    BT_CHK_NULL(bt_stream, ESMC_CONTEXT);

    struct bt_ctf_field *packet_context = bt_ctf_stream_get_packet_context(bt_stream);
    BT_CHK_NULL(packet_context, ESMC_CONTEXT);

    struct bt_ctf_field *field_pet = bt_ctf_field_structure_get_field(packet_context, "pet");
    BT_CHK_NULL(field_pet, ESMC_CONTEXT);
    
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_pet, stream_id), ESMC_CONTEXT);

    BT_CHK(bt_ctf_writer_add_environment_field(bt_writer, "esmf_trace_version", BT_ESMF_TRACE_VERSION), ESMC_CONTEXT);

    //reference counting
    bt_ctf_field_put(packet_context);
    bt_ctf_field_put(field_pet);    
    
  }


  void TraceClose(int *rc) {

    if (rc != NULL) *rc = ESMF_SUCCESS;

    bt_ctf_writer_flush_metadata(bt_writer);
    bt_ctf_writer_put(bt_writer);
    bt_ctf_stream_class_put(bt_stream_class);
    bt_ctf_stream_put(bt_stream);

    bt_ctf_field_type_put(bt_type_uint4);
    bt_ctf_field_type_put(bt_type_uint8);
    bt_ctf_field_type_put(bt_type_uint16);
    bt_ctf_field_type_put(bt_type_uint32);
    bt_ctf_field_type_put(bt_type_enum_control);
    bt_ctf_field_type_put(bt_type_enum_method);
    bt_ctf_field_type_put(bt_type_seq_attr);
    
    bt_ctf_event_class_put(bt_event_class_control); 
     
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceSetupTypes()" 
  void TraceSetupTypes(int *rc) {

    if (rc != NULL) *rc = ESMF_SUCCESS;

    //general types
    bt_type_uint32 = bt_ctf_field_type_integer_create(32);
    BT_CHK_NULL(bt_type_uint32, ESMC_CONTEXT);
    BT_CHK(bt_ctf_field_type_set_alignment(bt_type_uint32, 8), ESMC_CONTEXT);
    
    bt_type_uint16 = bt_ctf_field_type_integer_create(16);
    BT_CHK_NULL(bt_type_uint16, ESMC_CONTEXT);
    BT_CHK(bt_ctf_field_type_set_alignment(bt_type_uint16, 8), ESMC_CONTEXT);
    
    bt_type_uint8 = bt_ctf_field_type_integer_create(8);
    BT_CHK_NULL(bt_type_uint8, ESMC_CONTEXT);
    BT_CHK(bt_ctf_field_type_set_alignment(bt_type_uint8, 8), ESMC_CONTEXT);
        
    bt_type_uint4 = bt_ctf_field_type_integer_create(4);
    BT_CHK_NULL(bt_type_uint4, ESMC_CONTEXT);

    bt_type_string = bt_ctf_field_type_string_create();
    BT_CHK_NULL(bt_type_string, ESMC_CONTEXT);
    
    //define stream class
    bt_stream_class = bt_ctf_stream_class_create("esmf_stream");
    BT_CHK_NULL(bt_stream_class, ESMC_CONTEXT);
    
    BT_CHK(bt_ctf_stream_class_set_clock(bt_stream_class, bt_clock), ESMC_CONTEXT);

    struct bt_ctf_field_type *packet_context_type =
      bt_ctf_stream_class_get_packet_context_type(bt_stream_class);

    BT_CHK(bt_ctf_field_type_structure_add_field(packet_context_type, bt_type_uint32, "pet"), ESMC_CONTEXT);
    
    //control event
    bt_event_class_control = bt_ctf_event_class_create("control");
    BT_CHK_NULL(bt_event_class_control, ESMC_CONTEXT);
           
    bt_type_enum_control = bt_ctf_field_type_enumeration_create(bt_type_uint4);
    BT_CHK_NULL(bt_type_enum_control, ESMC_CONTEXT);

    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_control, "start_phase", BT_CNTL_START, BT_CNTL_START);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_control, "end_phase", BT_CNTL_END, BT_CNTL_END);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_control, "start_prologue", BT_CNTL_STARTP, BT_CNTL_STARTP);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_control, "end_prologue", BT_CNTL_ENDP, BT_CNTL_ENDP);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_control, "start_epilogue", BT_CNTL_STARTE, BT_CNTL_STARTE);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_control, "end_epilogue", BT_CNTL_ENDE, BT_CNTL_ENDE);
            
    bt_type_enum_method = bt_ctf_field_type_enumeration_create(bt_type_uint4);
    BT_CHK_NULL(bt_type_enum_method, ESMC_CONTEXT);

    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_method, "initialize", BT_METHOD_INIT, BT_METHOD_INIT);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_method, "run", BT_METHOD_RUN, BT_METHOD_RUN);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_method, "finalize", BT_METHOD_FINAL, BT_METHOD_FINAL);
            
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_control, bt_type_uint32, "vmid"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_control, bt_type_uint32, "baseid"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_control, bt_type_enum_control, "ctrl"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_control, bt_type_enum_method, "method"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_control, bt_type_uint8, "phase"), ESMC_CONTEXT);
    
    BT_CHK(bt_ctf_stream_class_add_event_class(bt_stream_class, bt_event_class_control), ESMC_CONTEXT);    

    bt_event_class_comp = bt_ctf_event_class_create("comp");
    BT_CHK_NULL(bt_event_class_comp, ESMC_CONTEXT);

    //attribute pack
    struct bt_ctf_field_type *bt_type_struct_attr = bt_ctf_field_type_structure_create();
    BT_CHK_NULL(bt_type_struct_attr, ESMC_CONTEXT);

    BT_CHK(bt_ctf_field_type_structure_add_field(bt_type_struct_attr, bt_type_string, "key"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_field_type_structure_add_field(bt_type_struct_attr, bt_type_string, "value"), ESMC_CONTEXT);

    bt_type_seq_attr = bt_ctf_field_type_sequence_create(bt_type_struct_attr, "attr_len");
    BT_CHK_NULL(bt_type_seq_attr, ESMC_CONTEXT);

    //component
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_comp, bt_type_uint32, "vmid"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_comp, bt_type_uint32, "baseid"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_comp, bt_type_string, "name"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_comp, bt_type_uint16, "attr_len"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_comp, bt_type_seq_attr, "attributes"), ESMC_CONTEXT);   
    
    BT_CHK(bt_ctf_stream_class_add_event_class(bt_stream_class, bt_event_class_comp), ESMC_CONTEXT);    
    
    //reference management
    bt_ctf_field_type_put(packet_context_type);
    bt_ctf_field_type_put(bt_type_struct_attr);

    
  }


        
  void TraceEventPhase
     (
      int ctrl,
      int *ep_vmid,
      int *ep_baseid,
      int *ep_method,
      int *ep_phase
      )
  {

    int localrc;
    int *rc = &localrc;
    
    struct bt_ctf_event *event = bt_ctf_event_create(bt_event_class_control);

    struct bt_ctf_field *field_ctrl = bt_ctf_field_create(bt_type_enum_control);
    struct bt_ctf_field *ctrl_container_field = bt_ctf_field_enumeration_get_container(field_ctrl);
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(ctrl_container_field, ctrl), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_set_payload(event, "ctrl", field_ctrl), ESMC_CONTEXT);
    
    struct bt_ctf_field *field_vmid = bt_ctf_field_create(bt_type_uint32);
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_vmid, *ep_vmid), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_set_payload(event, "vmid", field_vmid), ESMC_CONTEXT);

    struct bt_ctf_field *field_baseid = bt_ctf_field_create(bt_type_uint32);
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_baseid, *ep_baseid), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_set_payload(event, "baseid", field_baseid), ESMC_CONTEXT);
    
    struct bt_ctf_field *field_method = bt_ctf_field_create(bt_type_enum_method);
    struct bt_ctf_field *method_container_field = bt_ctf_field_enumeration_get_container(field_method);
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(method_container_field, *ep_method), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_set_payload(event, "method", field_method), ESMC_CONTEXT);

    struct bt_ctf_field *field_phase = bt_ctf_field_create(bt_type_uint8);
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_phase, *ep_phase), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_set_payload(event, "phase", field_phase), ESMC_CONTEXT);
    
    BT_CHK(bt_ctf_clock_set_time(bt_clock, get_clock()), ESMC_CONTEXT);

    BT_CHK(bt_ctf_stream_append_event(bt_stream, event), ESMC_CONTEXT);
    BT_CHK(bt_ctf_stream_flush(bt_stream), ESMC_CONTEXT);

    bt_ctf_event_put(event);
    bt_ctf_field_put(field_ctrl);
    bt_ctf_field_put(ctrl_container_field);
    bt_ctf_field_put(field_vmid);
    bt_ctf_field_put(field_baseid);
    bt_ctf_field_put(field_method);
    bt_ctf_field_put(method_container_field);
    bt_ctf_field_put(field_phase);
    
  }
  
  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    TraceEventPhase(BT_CNTL_START, ep_vmid, ep_baseid, ep_method, ep_phase);
  }
  
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    TraceEventPhase(BT_CNTL_END, ep_vmid, ep_baseid, ep_method, ep_phase);
  }

  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    TraceEventPhase(BT_CNTL_STARTP, ep_vmid, ep_baseid, ep_method, ep_phase);
  }

  void TraceEventPhasePrologueExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    TraceEventPhase(BT_CNTL_ENDP, ep_vmid, ep_baseid, ep_method, ep_phase);
  }

  void TraceEventPhaseEpilogueEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    TraceEventPhase(BT_CNTL_STARTE, ep_vmid, ep_baseid, ep_method, ep_phase);
  }

  void TraceEventPhaseEpilogueExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    TraceEventPhase(BT_CNTL_ENDE, ep_vmid, ep_baseid, ep_method, ep_phase);
  }

  void TraceEventComponentInfo
  (
   Comp *comp,
   int *ep_vmid,
   int *ep_baseid,
   const char *ep_name,
   std::string attributeKeys,
   std::string attributeVals
   )
  {
    
    int localrc;
    int *rc = &localrc;

    //printf("ESMCI::TraceEventComponentInfo: \n\t%s\n\t%s", attributeKeys.c_str(), attributeVals.c_str());
    
    struct bt_ctf_event *event = bt_ctf_event_create(bt_event_class_comp);

    struct bt_ctf_field *field_vmid = bt_ctf_event_get_payload(event, "vmid");
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_vmid, *ep_vmid), ESMC_CONTEXT);

    struct bt_ctf_field *field_baseid = bt_ctf_event_get_payload(event, "baseid");
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_baseid, *ep_baseid), ESMC_CONTEXT);
    
    struct bt_ctf_field *field_name = bt_ctf_event_get_payload(event, "name");
    BT_CHK(bt_ctf_field_string_set_value(field_name, ep_name), ESMC_CONTEXT);
 
    const vector<string> attrKeys = split(attributeKeys, "::");
    const vector<string> attrVals = split(attributeVals, "::");

    struct bt_ctf_field *field_attr_len = bt_ctf_event_get_payload(event, "attr_len");
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_attr_len, attrKeys.size()), ESMC_CONTEXT);
    
    struct bt_ctf_field *field_seq_attr = bt_ctf_event_get_payload(event, "attributes");
    BT_CHK(bt_ctf_field_sequence_set_length(field_seq_attr, field_attr_len), ESMC_CONTEXT);

    for (int i = 0; i < attrKeys.size(); i++) {
      struct bt_ctf_field *field_attr = bt_ctf_field_sequence_get_field(field_seq_attr, i);
      BT_CHK_NULL(field_attr, ESMC_CONTEXT);
      
      struct bt_ctf_field *field_attr_key = bt_ctf_field_structure_get_field(field_attr, "key");
      BT_CHK_NULL(field_attr_key, ESMC_CONTEXT);
      
      struct bt_ctf_field *field_attr_value = bt_ctf_field_structure_get_field(field_attr, "value");
      BT_CHK_NULL(field_attr_value, ESMC_CONTEXT);
      
      BT_CHK(bt_ctf_field_string_set_value(field_attr_key, attrKeys.at(i).c_str()), ESMC_CONTEXT);
      BT_CHK(bt_ctf_field_string_set_value(field_attr_value, attrVals.at(i).c_str()), ESMC_CONTEXT);
            
      bt_ctf_field_put(field_attr);
      bt_ctf_field_put(field_attr_key);
      bt_ctf_field_put(field_attr_value);
    }
        
    BT_CHK(bt_ctf_clock_set_time(bt_clock, get_clock()), ESMC_CONTEXT);
    
    BT_CHK(bt_ctf_stream_append_event(bt_stream, event), ESMC_CONTEXT);
    BT_CHK(bt_ctf_stream_flush(bt_stream), ESMC_CONTEXT);

    bt_ctf_event_put(event);
    bt_ctf_field_put(field_name);
    bt_ctf_field_put(field_vmid);
    bt_ctf_field_put(field_baseid);
    bt_ctf_field_put(field_attr_len);
    
  }

    
  
}

/* archive this here for now */

/*

//#include <esmftrc.h>

//#ifdef __cplusplus
//# define TO_VOID_PTR(_value)		static_cast<void *>(_value)
//# define FROM_VOID_PTR(_type, _value)	static_cast<_type *>(_value)
//#else
//# define TO_VOID_PTR(_value)		((void *) (_value))
//# define FROM_VOID_PTR(_type, _value)	((_type *) (_value))
//#endif

  struct esmftrc_platform_filesys_ctx {
    struct esmftrc_default_ctx ctx;
    FILE *fh;
    int stream_id;
  };
  
  //global context
  static struct esmftrc_platform_filesys_ctx *g_esmftrc_platform_filesys_ctx;
  
  static uint64_t get_clock(void* data)
  {
    struct timespec ts;
    
    //clock_gettime(CLOCK_MONOTONIC, &ts);
    clock_gettime(CLOCK_REALTIME, &ts);

    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
  }
  
  static void write_packet(struct esmftrc_platform_filesys_ctx *ctx)
  {
    size_t nmemb = fwrite(esmftrc_packet_buf(&ctx->ctx),
			  esmftrc_packet_buf_size(&ctx->ctx), 1, ctx->fh);
    assert(nmemb == 1);
  }

  static int is_backend_full(void *data)
  {
    //struct esmftrc_platform_linux_fs_ctx *ctx =
    // FROM_VOID_PTR(struct esmftrc_platform_linux_fs_ctx, data);
    
    // assume file-based backend is never full
    return 0;
  }

  static void open_packet(void *data)
  {
    struct esmftrc_platform_filesys_ctx *ctx =
      FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, data);

    esmftrc_default_open_packet(&ctx->ctx, ctx->stream_id);
  }

  static void close_packet(void *data)
  {
    struct esmftrc_platform_filesys_ctx *ctx =
      FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, data);

    // close packet now
    esmftrc_default_close_packet(&ctx->ctx);

    // write packet to file 
    write_packet(ctx);
  }
  
// #undef  ESMC_METHOD
// #define ESMC_METHOD "ESMCI::TraceOpen()"  
//   void TraceOpen(unsigned int buf_size, const char *trace_dir, int stream_id, int *rc) {
    
//     int localrc;
//     char stream_path[ESMC_MAXPATHLEN];
//     char stream_dir[ESMC_MAXPATHLEN];
//     uint8_t *buf;
//     struct esmftrc_platform_filesys_ctx *ctx;
//     struct esmftrc_platform_callbacks cbs;

//     if (rc != NULL) *rc = ESMF_SUCCESS;

//     cbs.sys_clock_clock_get_value = get_clock;
//     cbs.is_backend_full = is_backend_full;
//     cbs.open_packet = open_packet;
//     cbs.close_packet = close_packet;
//     ctx = FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, malloc(sizeof(*ctx)));
    
//     ESMC_LogDefault.Write("Enabling ESMF Tracing", ESMC_LOGMSG_INFO);
//     if (!ctx) {
//       ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate context", 
//                                     ESMC_CONTEXT, rc);
//       return;
//     }
  
//     buf = FROM_VOID_PTR(uint8_t, malloc(buf_size));

//     if (!buf) {
//       free(ctx);
//       ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate trace event buffer", 
//                                     ESMC_CONTEXT, rc);
//       return;
//     }
    
//     memset(buf, 0, buf_size);
    
//     //make relative path absolute if needed
//     if (trace_dir[0] != '/') {
//       char cwd[1024];
//       if (getcwd(cwd, sizeof(cwd)) == NULL) {
//         ESMC_LogDefault.MsgFoundError(ESMC_RC_SYS, "Error getting working directory", 
//                                       ESMC_CONTEXT, rc);
//         return; 
//       }
//       sprintf(stream_dir, "%s/%s", cwd, trace_dir);
//       //printf("absolute dir = |%s|\n", stream_dir);
//     }
//     else {
//       sprintf(stream_dir, "%s", trace_dir);
//     }
           
//     //root stream responsible for creating trace directory     
//     if (stream_id == 0) {      
//       struct stat st = {0};
//       if (stat(stream_dir, &st) == -1) {
//         if (mkdir(stream_dir, 0700) == -1) {
//           //printf("mkdir == -1\n");
//           //perror("mkdir()");
//           ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error creating trace directory", 
//                                         ESMC_CONTEXT, rc);
//           return;
//         }
//       }
//     }

//     //all PETs wait for directory to be created
//     VM *globalvm = VM::getGlobal(&localrc);
//     if (ESMC_LogDefault.MsgFoundError(localrc, 
//         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
//       return;
//     globalvm->barrier();
    
//     sprintf(stream_path, "%s/stream%d", stream_dir, stream_id);
//     ctx->fh = fopen(stream_path, "wb");

//     if (!ctx->fh) {
//       free(ctx);
//       free(buf);
//       ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN, "Error opening trace output file", 
//                                     ESMC_CONTEXT, rc);
//       return;
//     }

//     ctx->stream_id = stream_id;

//     esmftrc_init(&ctx->ctx, buf, buf_size, cbs, ctx);
//     open_packet(ctx);

//     //store as global context
//     g_esmftrc_platform_filesys_ctx = ctx;

//   }

   
  void TraceClose(int *rc)
  {
    struct esmftrc_platform_filesys_ctx *ctx = g_esmftrc_platform_filesys_ctx;
    
    if(rc != NULL) rc = ESMF_SUCCESS;
 
    if (esmftrc_packet_is_open(&ctx->ctx) &&
	!esmftrc_packet_is_empty(&ctx->ctx)) {
      close_packet(ctx);
    }

    fclose(ctx->fh);
    free(esmftrc_packet_buf(&ctx->ctx));
    free(ctx);
  }
  

  
  struct esmftrc_default_ctx *esmftrc_platform_linux_fs_get_esmftrc_ctx(
									struct esmftrc_platform_linux_fs_ctx *ctx)
  {
    return &ctx->ctx;
  }
  

  static struct esmftrc_default_ctx *esmftrc_platform_get_default_ctx()
  {
    return &g_esmftrc_platform_filesys_ctx->ctx;
  }
  

 */
