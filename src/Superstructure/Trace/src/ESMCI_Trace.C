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

/*
 * Copyright (c) 2015 EfficiOS Inc. and Linux Foundation
 * Copyright (c) 2015 Philippe Proulx <pproulx@efficios.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_Trace.h"

#include <esmftrc.h>

#ifdef __cplusplus
# define TO_VOID_PTR(_value)		static_cast<void *>(_value)
# define FROM_VOID_PTR(_type, _value)	static_cast<_type *>(_value)
#else
# define TO_VOID_PTR(_value)		((void *) (_value))
# define FROM_VOID_PTR(_type, _value)	((_type *) (_value))
#endif  

namespace ESMCI {

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

    /* close packet now */
    esmftrc_default_close_packet(&ctx->ctx);

    /* write packet to file */
    write_packet(ctx);
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceOpen()"  
  void TraceOpen(unsigned int buf_size, const char *trace_dir, int stream_id, int *rc) {
    
    int localrc;
    char stream_path[ESMC_MAXPATHLEN];
    char stream_dir[ESMC_MAXPATHLEN];
    uint8_t *buf;
    struct esmftrc_platform_filesys_ctx *ctx;
    struct esmftrc_platform_callbacks cbs;

    if (rc != NULL) *rc = ESMF_SUCCESS;

    cbs.sys_clock_clock_get_value = get_clock;
    cbs.is_backend_full = is_backend_full;
    cbs.open_packet = open_packet;
    cbs.close_packet = close_packet;
    ctx = FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, malloc(sizeof(*ctx)));
    
    ESMC_LogDefault.Write("Enabling ESMF Tracing", ESMC_LOGMSG_INFO);
    if (!ctx) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate context", 
                                    ESMC_CONTEXT, rc);
      return;
    }
  
    buf = FROM_VOID_PTR(uint8_t, malloc(buf_size));

    if (!buf) {
      free(ctx);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate trace event buffer", 
                                    ESMC_CONTEXT, rc);
      return;
    }
    
    memset(buf, 0, buf_size);
    
    //make relative path absolute if needed
    if (trace_dir[0] != '/') {
      char cwd[1024];
      if (getcwd(cwd, sizeof(cwd)) == NULL) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_SYS, "Error getting working directory", 
                                      ESMC_CONTEXT, rc);
        return; 
      }
      sprintf(stream_dir, "%s/%s", cwd, trace_dir);
      //printf("absolute dir = |%s|\n", stream_dir);
    }
    else {
      sprintf(stream_dir, "%s", trace_dir);
    }
           
    //root stream responsible for creating trace directory     
    if (stream_id == 0) {      
      struct stat st = {0};
      if (stat(stream_dir, &st) == -1) {
        if (mkdir(stream_dir, 0700) == -1) {
          //printf("mkdir == -1\n");
          //perror("mkdir()");
          ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error creating trace directory", 
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
    
    sprintf(stream_path, "%s/stream%d", stream_dir, stream_id);
    ctx->fh = fopen(stream_path, "wb");

    if (!ctx->fh) {
      free(ctx);
      free(buf);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN, "Error opening trace output file", 
                                    ESMC_CONTEXT, rc);
      return;
    }

    ctx->stream_id = stream_id;

    esmftrc_init(&ctx->ctx, buf, buf_size, cbs, ctx);
    open_packet(ctx);

    //store as global context
    g_esmftrc_platform_filesys_ctx = ctx;

  }

  
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

  /*
  struct esmftrc_default_ctx *esmftrc_platform_linux_fs_get_esmftrc_ctx(
									struct esmftrc_platform_linux_fs_ctx *ctx)
  {
    return &ctx->ctx;
  }
  */

  static struct esmftrc_default_ctx *esmftrc_platform_get_default_ctx()
  {
    return &g_esmftrc_platform_filesys_ctx->ctx;
  }
  

  ////////////////////////////////


  /*
   * These functions call into the generated tracer functions
   * in esmftrc.c.
   */

  void TraceEventPhaseEnter  
     (
     int *ep_vmid,
     int *ep_baseid,
     int *ep_method,
     int *ep_phase
     )
  {
    esmftrc_default_trace_phase_enter(esmftrc_platform_get_default_ctx(),
				      *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }
  
  void TraceEventPhaseExit 
    (
     int *ep_vmid,
     int *ep_baseid,
     int *ep_method,
     int *ep_phase
     )
  {
    esmftrc_default_trace_phase_exit(esmftrc_platform_get_default_ctx(),
				     *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventPhasePrologueEnter  
  (
     int *ep_vmid,
     int *ep_baseid,
     int *ep_method,
     int *ep_phase
     )
  {
    esmftrc_default_trace_phase_prologue_enter(esmftrc_platform_get_default_ctx(),
                                               *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventPhasePrologueExit  
     (
     int *ep_vmid,
     int *ep_baseid,
     int *ep_method,
     int *ep_phase
     )
  {
    esmftrc_default_trace_phase_prologue_exit(esmftrc_platform_get_default_ctx(),
                                              *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventPhaseEpilogueEnter  
  (
     int *ep_vmid,
     int *ep_baseid,
     int *ep_method,
     int *ep_phase
     )
  {
    esmftrc_default_trace_phase_epilogue_enter(esmftrc_platform_get_default_ctx(),
                                               *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventPhaseEpilogueExit  
     (
     int *ep_vmid,
     int *ep_baseid,
     int *ep_method,
     int *ep_phase
     )
  {
    esmftrc_default_trace_phase_epilogue_exit(esmftrc_platform_get_default_ctx(),
                                              *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventComponentInfo
  (
   int *ep_vmid,
   int *ep_baseid,
   const char *ep_name
   )
  {
    esmftrc_default_trace_component_info(esmftrc_platform_get_default_ctx(),
                                         *ep_vmid,
                                         *ep_baseid,
                                         ep_name);
  }

  
}
