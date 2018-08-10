// $Id$
/*
 * Writes trace events to the file system.
 *
 * Earth System Modeling Framework
 * Copyright 2002-2018, University Corporation for Atmospheric Research, 
 * Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
 * Laboratory, University of Michigan, National Centers for Environmental 
 * Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
 * NASA Goddard Space Flight Center.
 * Licensed under the University of Illinois-NCSA License.
 */

#include <iomanip>
#include <sstream>
#include <string>

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef ESMF_OS_MinGW
#include <unistd.h>
#else
#include <Winsock.h>
#endif

#ifndef ESMF_NO_DLFCN
#include <dlfcn.h>
#endif

#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_Trace.h"
#include "ESMCI_Comp.h"
#include "ESMCI_VMKernel.h"
#include "ESMCI_HashMap.h"
#include "ESMCI_RegionNode.h"
#include "ESMCI_ComponentInfo.h"
#include "ESMCI_TraceUtil.h"
#include <esmftrc.h>

#ifdef __cplusplus
# define TO_VOID_PTR(_value)           static_cast<void *>(_value)
# define FROM_VOID_PTR(_type, _value)  static_cast<_type *>(_value)
#else
# define TO_VOID_PTR(_value)           ((void *) (_value))
# define FROM_VOID_PTR(_type, _value)  ((_type *) (_value))
#endif

#ifndef ESMF_OS_MinGW
#define TRACE_DIR_PERMISSIONS (S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH)
#else
#define TRACE_DIR_PERMISSIONS (S_IRWXU)
#endif

#define NODENAME_LEN 100
#define EVENT_BUF_SIZE_DEFAULT 4096
#define EVENT_BUF_SIZE_EAGER 1024
#define REGION_HASHTABLE_SIZE 100

using std::string;
using std::vector;
using std::stringstream;

namespace ESMCI {

  struct StringHashF {
    unsigned long operator()(const string& s) const {
      unsigned long hash = 5381;
      int c;
      const char *str = s.c_str();     
      while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
      //printf("hash for %s = %d\n", s.c_str(), hash % REGION_HASHTABLE_SIZE);
      return hash % REGION_HASHTABLE_SIZE;
    }
  };

  struct ESMFPhaseHashF {
    unsigned long operator()(const ESMFPhaseId& phaseId) const {
      return phaseId.hashcode() % REGION_HASHTABLE_SIZE;
    }
  };

  struct ESMFIdHashF {
    unsigned long operator()(const ESMFId& esmfId) const {
      return esmfId.hashcode() % REGION_HASHTABLE_SIZE;
    }
  };

  static bool traceInitialized = false;
  static bool traceLocalPet = false;

  /*
    Timed regions are defined by either:
     1) a component phase represented by (vmid, baseid, method, phase), or
     2) a user-defined region name (from ESMF_TraceRegionEnter()/Exit() calls)

    Both are mapped to a single integer id for the region.  In the trace
    we output a "definition" event for each region, which includes the
    region id and either the user-defined name or the (vmid, baseid, method, phase)
    tuple.  The trace post-processor can then translate the ids back to
    meaningful names.  Using only integer ids reduces the size of the trace files.

    userRegionMap:  maps from user-defined name to region id
    phaseRegionMap:  maps from phase (vmid,baseid,method,phase) to region id
    
    componentInfoMap:  maps from (vmid,baseid) to an object
                       for keeping track of component information

   */

  static HashMap<string, int, REGION_HASHTABLE_SIZE, StringHashF> userRegionMap;
  static HashMap<ESMFPhaseId, int, REGION_HASHTABLE_SIZE, ESMFPhaseHashF> phaseRegionMap;
  static HashMap<ESMFId, ComponentInfo *, REGION_HASHTABLE_SIZE, ESMFIdHashF> componentInfoMap;
  
  static int next_region_id() {
    static int next = 0;
    return next++;
  } 

  static bool profileActive = true;
  static RegionNode rootRegionNode(next_region_id());
  static RegionNode *currentRegionNode = &rootRegionNode;
  
#ifndef ESMF_NO_DLFCN
  static int (*notify_wrappers)(int initialized) = NULL;
#endif

  //this data structure used to map VMIds(vmKey,localid) to an integer id
#define VMID_MAP_SIZE 10000
  static VMId vmIdMap[VMID_MAP_SIZE];
  static int nextVmId = 0;

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceMapVmId()"  
  int TraceMapVmId(VMId *vmid, int *rc) {

    int localrc = ESMC_RC_NOT_IMPL;
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    
    int foundIdx;
    //search backward - vm more likely to be at the end
    for (foundIdx=nextVmId-1; foundIdx >= 0; foundIdx--) {
      if (VMIdCompare(vmid, &(vmIdMap[foundIdx]))) {
        break;
      }
    }
    if (foundIdx >= 0) {
      if (rc!=NULL) *rc = ESMF_SUCCESS;
      return foundIdx;
    }
    else {
      if (nextVmId >= VMID_MAP_SIZE) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      "Out of VmIdMap space inside tracing", ESMC_CONTEXT, rc);
        return -1;
      }
      else {
        localrc = (&vmIdMap[nextVmId])->create ();
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU,
                                          ESMC_CONTEXT, rc))
          return -1;
        VMIdCopy(&(vmIdMap[nextVmId]), vmid);
        foundIdx = nextVmId;
        nextVmId++;

        if (rc!=NULL) *rc=ESMF_SUCCESS;
        return foundIdx;
      }
    }        
  }
  
  bool TraceInitialized() {
    return traceInitialized;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceIsEnabledForPET()"  
  bool TraceIsEnabledForPET(int *rc){
    
    int localrc;
    if (rc != NULL) *rc = ESMF_SUCCESS;
    
    //first check if tracing is enabled
    int tracingEnabled = 0;
    localrc = Comp::getComplianceCheckerTrace(&tracingEnabled);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
      return false;

    if (tracingEnabled == 0) return false;
    //printf("Tracing enabled\n");
    
    //then check if local PET is enabled 
    VM *globalvm = VM::getGlobal(&localrc);
    int localPet = globalvm->getMypet();
    bool invalidFormat = false;
    
    char const *envVar = VM::getenv("ESMF_RUNTIME_TRACE_PETLIST");
    if (envVar != NULL && strlen(envVar) > 0) {     
      std::string envStr(envVar);
      const vector<string> listItems = split(trim(envStr), " ");
      
      for (unsigned i = 0; i < listItems.size(); i++) {
	if (listItems.at(i).find("-") != string::npos) {	  
	  vector<string> petRange = split(trim(listItems.at(i)), "-");
	  if (petRange.size() == 2) {
	    int low, high;
	    stringstream lowss(trim(petRange.at(0)));
	    stringstream highss(trim(petRange.at(1)));
	    if (!(lowss >> low).fail() && !(highss >> high).fail()) {
	      //printf("low=%d, high=%d\n", low, high);
	      if (localPet >= low && localPet <= high) {
                return true;
	      }
	    }
	    else {
	      invalidFormat = true;
	    }
	  }
	  else {
	    invalidFormat = true;
	  }
	}
	else {  
	  int pet = -1;
	  stringstream ss(trim(listItems.at(i)));
	  ss >> pet;
	  
	  if(!(ss.fail())) {
	    if (localPet == pet) {
              return true;
	    }
	  }	
	  else {
	    invalidFormat = true;
	  }
	  
	}
        
	if (invalidFormat) {
	  ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
					"Invalid format in env variable ESMF_RUNTIME_TRACE_PETLIST", 
					ESMC_CONTEXT, rc);
	  if (rc != NULL) *rc = ESMF_SUCCESS;
	  return false;
	}
		
      }
      
      //pet not found in list      
      if (localPet == 0) {
	//pet 0 always participates in trace
	return true;
      }
      else {
        return false;
      }
   
    }
    else { //no env variable defined, so trace all pets
      return true;
    }
    
  }

  struct esmftrc_platform_filesys_ctx {
    struct esmftrc_default_ctx ctx;
    FILE *fh;
    int stream_id;
    char nodename[NODENAME_LEN];
  };
  
  //global context
  static struct esmftrc_platform_filesys_ctx *g_esmftrc_platform_filesys_ctx = NULL;

  static struct esmftrc_default_ctx *esmftrc_platform_get_default_ctx() {
    return &g_esmftrc_platform_filesys_ctx->ctx;
  }
  
  static void write_packet(struct esmftrc_platform_filesys_ctx *ctx) {
    size_t nmemb = fwrite(esmftrc_packet_buf(&ctx->ctx),
			  esmftrc_packet_buf_size(&ctx->ctx), 1, ctx->fh);
    assert(nmemb == 1);
  }

  static int is_backend_full(void *data) {
    //assume file system never full
    return 0;
  }

  static void open_packet(void *data) {
    struct esmftrc_platform_filesys_ctx *ctx =
      FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, data);
    
    esmftrc_default_open_packet(&ctx->ctx, ctx->nodename, ctx->stream_id);
  }

  static void close_packet(void *data) {
    struct esmftrc_platform_filesys_ctx *ctx =
      FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, data);
    
    // close packet now
    esmftrc_default_close_packet(&ctx->ctx);
    
    // write packet to file 
    write_packet(ctx);
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::write_metadata()"  
  static void write_metadata(const char *trace_dir, int *rc) {

    *rc = ESMF_SUCCESS;
    
    string metadata_string = TraceGetMetadataString();
    string filename(trace_dir);
    filename += "/metadata";
    
    std::ofstream ofs (filename.c_str(), std::ofstream::trunc);
    if (ofs.is_open() && !ofs.fail()) {
      ofs << metadata_string;
      ofs.close();
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error writing trace metadata file", 
                                    ESMC_CONTEXT, rc);
    }
  }

  static void PushIOStats();
  static void PopIOStats();
  static void PushMPIStats();
  static void PopMPIStats();
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceOpen()"  
  void TraceOpen(std::string trace_dir, int *rc) {

    int localrc;
    std::string stream_dir_root;
    struct esmftrc_platform_filesys_ctx *ctx;
    struct esmftrc_platform_callbacks cbs;
    uint8_t *buf;
    std::stringstream logMsg;
    
    if (rc != NULL) *rc = ESMF_SUCCESS;

    VM *globalvm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
      return;
    int stream_id = globalvm->getLocalPet();

    TraceInitializeClock(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
      return;
    
    //determine which clock to use
    /*
    traceClock = ESMF_CLOCK_REALTIME;  //default
    std::string strClk = "REALTIME";   //default
    
    char const *envClk = VM::getenv("ESMF_RUNTIME_TRACE_CLOCK");
    if (envClk != NULL && strlen(envClk) > 0) {     
      strClk = envClk;
      if (strClk == "REALTIME") {
        traceClock = ESMF_CLOCK_REALTIME;
      }
      else if (strClk == "MONOTONIC") {
        traceClock = ESMF_CLOCK_MONOTONIC;
      }
      else if (strClk == "MONOTONIC_SYNC") {
        traceClock = ESMF_CLOCK_MONOTONIC_SYNC;
      }
    }

    //determine local offsets if requested
    if (traceClock == ESMF_CLOCK_MONOTONIC_SYNC) {
      traceClockOffset = clock_sync_offset(&localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, 
           ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
        return;
      }
      //printf("[%d] local offset = %lld\n", stream_id, traceClockOffset);
    }

    std::stringstream logMsg;
    logMsg << "ESMF Tracing enabled using clock: " + strClk;
    if (traceClock == ESMF_CLOCK_MONOTONIC_SYNC) {
      logMsg << " (local offset = " << traceClockOffset << ")";
    }
    ESMC_LogDefault.Write(logMsg.str().c_str(), ESMC_LOGMSG_INFO);
    */
    
    // set up callbacks
    cbs.sys_clock_clock_get_value = TraceGetClock;  //get_clock;
    cbs.is_backend_full = is_backend_full;
    cbs.open_packet = open_packet;
    cbs.close_packet = close_packet;
    ctx = FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, malloc(sizeof(*ctx))); 
    if (!ctx) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate context", 
                                    ESMC_CONTEXT, rc);
      return;
    }

    //allocate event buffer
    char const *envFlush = VM::getenv("ESMF_RUNTIME_TRACE_FLUSH");
    std::string strFlush = "DEFAULT";
    int eventBufSize = EVENT_BUF_SIZE_DEFAULT;
            
    if (envFlush != NULL && strlen(envFlush) > 0) {     
      strFlush = envFlush;
      if (trim(strFlush) == "EAGER" || trim(strFlush) == "eager" || trim(strFlush) == "Eager") {
        eventBufSize = EVENT_BUF_SIZE_EAGER;
        logMsg.str("ESMF Tracing set to EAGER flushing.");
        ESMC_LogDefault.Write(logMsg.str().c_str(), ESMC_LOGMSG_INFO);
      }
    }
    
    buf = FROM_VOID_PTR(uint8_t, malloc(eventBufSize));
    if (!buf) {
      free(ctx);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate trace event buffer", 
                                    ESMC_CONTEXT, rc);
      return;
    }
    
    memset(buf, 0, eventBufSize); 
    
    //make relative path absolute if needed
    if (trace_dir[0] != '/') {
      char cwd[ESMC_MAXPATHLEN];
      FTN_X(c_esmc_getcwd)(cwd, &localrc, ESMC_MAXPATHLEN);
      if (ESMC_LogDefault.MsgFoundError(localrc,
    		  "Error getting working directory", ESMC_CONTEXT, rc))
            return;
      stream_dir_root = string (cwd, ESMC_F90lentrim(cwd, ESMC_MAXPATHLEN)) + "/" + trace_dir;
    }
    else {
      stream_dir_root = trace_dir;
    }
    
    struct stat st;
    if (stream_id == 0) {
      if (stat(stream_dir_root.c_str(), &st) == -1) {
        
        ESMC_Logical relaxedFlag = ESMF_TRUE;
        int dir_perms = TRACE_DIR_PERMISSIONS;
        FTN_X(c_esmc_makedirectory)(stream_dir_root.c_str(), &dir_perms,
                                    &relaxedFlag, &localrc, stream_dir_root.length());
        
        if (ESMC_LogDefault.MsgFoundError(localrc,
                 "Error creating trace root directory", ESMC_CONTEXT, rc))
          return;
      }
    }
        
    //all PETs wait for directory to be created
    globalvm->barrier();
           
    //determine if tracing turned on for this PET
    traceLocalPet = TraceIsEnabledForPET(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
      traceLocalPet = false;
      return;
    }
       
    if (!traceLocalPet) return;

    //my specific file
    std::stringstream stream_file;
    stream_file << stream_dir_root << "/esmf_stream_" << std::setfill('0') << std::setw(4) << stream_id;
    
    ctx->fh = fopen(stream_file.str().c_str(), "wb");
    if (!ctx->fh) {
      free(ctx);
      free(buf);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN, "Error opening trace output file", 
                                    ESMC_CONTEXT, rc);
      return;
    }

    //get node name        
    if (gethostname(ctx->nodename, NODENAME_LEN) < 0) {
      ctx->nodename[0] = '\0';
    }
    
    ctx->stream_id = stream_id;

    //stream zero writes the metadata file
    if (stream_id == 0) {
      write_metadata(stream_dir_root.c_str(), &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, 
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
        return;
      }
    }
    
    esmftrc_init(&ctx->ctx, buf, eventBufSize, cbs, ctx);
    open_packet(ctx);

    //store as global context
    g_esmftrc_platform_filesys_ctx = ctx;

    PushIOStats();  /* needed in case there are I/O events before first region */
    PushMPIStats();
    traceInitialized = true;

    int wrappersPresent = TRACE_WRAP_NONE;
#ifndef ESMF_NO_DLFCN
    void *preload_lib = dlopen(NULL, RTLD_LAZY);
    if (preload_lib == NULL) {
      ESMC_LogDefault.Write("ESMF Tracing could not open shared library containing instrumentation.", ESMC_LOGMSG_WARN);
    }
    else {
      notify_wrappers = (int (*)(int)) dlsym(preload_lib, "c_esmftrace_notify_wrappers");
      if (notify_wrappers != NULL) {
	wrappersPresent = notify_wrappers(1);
      }
      else {
        ESMC_LogDefault.Write("ESMF Tracing could not load dynamic instrumentation functions.", ESMC_LOGMSG_WARN);
      }
    }
#else
    wrappersPresent = c_esmftrace_notify_wrappers(1);
#endif

    if (wrappersPresent != TRACE_WRAP_NONE) {
      logMsg.str("");
      logMsg << "ESMF Tracing enabled with "; 
      if (wrappersPresent == TRACE_WRAP_DYNAMIC) {
        logMsg << "DYNAMIC";
      }
      else if (wrappersPresent == TRACE_WRAP_STATIC) {
        logMsg << "STATIC";
      }
      logMsg << " instrumentation.  This option should only be used for profiling applications and NOT for production runs.";
      ESMC_LogDefault.Write(logMsg.str().c_str(), ESMC_LOGMSG_INFO);
    }
    
  }
  
#define STATLINE 256
  static void printProfile(RegionNode *rn, bool printToLog, std::string prefix) {
    if (rn->getParent() != NULL) {
      char strname[50];
      char strbuf[STATLINE];

      //snprintf(strname, 10, "%d", rn->getId());
      string name("UNKNOWN");
      if (rn->isUserRegion()) {
        bool present = userRegionMap.reverse(rn->getId(), name);
        if (!present) {
          name = "UNKNOWN_USER_REGION";
        }
      }
      else {
        ESMFPhaseId phaseId;
        bool present = phaseRegionMap.reverse(rn->getId(), phaseId);
        if (present) {
          ComponentInfo *ci = NULL;
          bool present = componentInfoMap.get(phaseId.getESMFId(), ci);
          if (present && ci != NULL) {
            name = ci->getPhaseName(phaseId);
          }
        }
      }
      name.insert(0, prefix);
      
      snprintf(strbuf, STATLINE, "%-35s %-6lu %-11.4f %-11.4f %-11.4f %-11.4f %-11.4f %-11.4f",
               name.c_str(), rn->getCount(), rn->getTotal()*NANOS_TO_MILLIS,
               rn->getSelfTime()*NANOS_TO_MILLIS, rn->getMean()*NANOS_TO_MILLIS,
               rn->getMin()*NANOS_TO_MILLIS, rn->getMax()*NANOS_TO_MILLIS, rn->getStdDev()*NANOS_TO_MILLIS);
      if (printToLog) {
        ESMC_LogDefault.Write(strbuf, ESMC_LOGMSG_INFO);
      }
      else {
        std::cout << std::string(strbuf) << "\n";
      }
    } 
    for (unsigned i = 0; i < rn->getChildren().size(); i++) {
      printProfile(rn->getChildren().at(i), printToLog, prefix + "  ");
    }
  }
  
  static void printProfile(bool printToLog) {
    char strbuf[STATLINE];
    snprintf(strbuf, STATLINE, "%-35s %-6s %-11s %-11s %-11s %-11s %-11s %-11s",
             "Region", "Count", "Total (ms)", "Self (ms)", "Mean (ms)", "Min (ms)", "Max (ms)", "Std. Dev. (ms)");
    if (printToLog) {
      ESMC_LogDefault.Write("**************** Region Timings *******************", ESMC_LOGMSG_INFO);
      ESMC_LogDefault.Write(strbuf, ESMC_LOGMSG_INFO);
    }
    else {
      std::cout << std::string(strbuf) << "\n";
    }
    printProfile(&rootRegionNode, printToLog, "");
  }
  

  

  
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceClose()"
  void TraceClose(int *rc) {
    struct esmftrc_platform_filesys_ctx *ctx = g_esmftrc_platform_filesys_ctx;
    
    if(rc != NULL) *rc = ESMF_SUCCESS;

    // allow multiple calls to TraceClose for system test
    // ignore any call after the first one
    
    if (ctx != NULL) {
      
      traceInitialized = false;
#ifndef ESMF_NO_DLFCN      
      if (notify_wrappers != NULL) {
	notify_wrappers(0);
      }
#else
      c_esmftrace_notify_wrappers(0);
#endif
      
      PopIOStats();
      PopMPIStats();
      
      if (esmftrc_packet_is_open(&ctx->ctx) &&
          !esmftrc_packet_is_empty(&ctx->ctx)) {
        close_packet(ctx);
      }
      
      fclose(ctx->fh);
      free(esmftrc_packet_buf(&ctx->ctx));
      free(ctx);
      g_esmftrc_platform_filesys_ctx = NULL;

      
      if (profileActive) {
        //std::cout << "PROFILE TREE:\n";
        //rootRegionNode.printProfile(true);
        printProfile(true);
      }
      
    }
    
  }

   
  ///////////////////// I/O Tracing //////////////////

  static std::string openFilename;
  static uint64_t openStartTimestamp = -1;

  void TraceIOOpenStart(const char *path) {
    if (!traceLocalPet) return;    
    openStartTimestamp = TraceGetClock(NULL);
    openFilename = string(path);
  }
  
  void TraceIOOpenEnd() {
    if (!traceLocalPet) return;
    uint64_t openEndTimestamp = TraceGetClock(NULL);
    uint64_t openTime = openEndTimestamp - openStartTimestamp;

    esmftrc_default_trace_ioopen(esmftrc_platform_get_default_ctx(),
                                 openFilename.c_str(), openTime);

    openStartTimestamp = -1;
  }

  void TraceIOCloseStart() {
  }
  
  void TraceIOCloseEnd() {
  }

  static vector<size_t> writeTotalBytes;
  static vector<uint64_t> writeTotalTime;
  static uint64_t writeStartTimestamp = -1;
    
  void TraceIOWriteStart() {
    writeStartTimestamp = TraceGetClock(NULL);
  }

  void TraceIOWriteEnd(size_t nbytes) {
    uint64_t writeEndTimestamp = TraceGetClock(NULL);
    writeTotalTime.back() = writeTotalTime.back() + (writeEndTimestamp - writeStartTimestamp);
    writeTotalBytes.back() = writeTotalBytes.back() + nbytes;
    writeStartTimestamp = -1;
  }

  static vector<size_t> readTotalBytes;
  static vector<uint64_t> readTotalTime;
  static uint64_t readStartTimestamp = -1;
  
  void TraceIOReadStart() {
    readStartTimestamp = TraceGetClock(NULL);
  }

  void TraceIOReadEnd(size_t nbytes) {
    uint64_t readEndTimestamp = TraceGetClock(NULL);
    readTotalTime.back() = readTotalTime.back() + (readEndTimestamp - readStartTimestamp);
    readTotalBytes.back() = readTotalBytes.back() + nbytes;
    readStartTimestamp = -1;
  }

  static void PushIOStats() {
    readTotalBytes.push_back(0);
    readTotalTime.push_back(0);
    writeTotalBytes.push_back(0);
    writeTotalTime.push_back(0);
  }

  static void PopIOStats() {

    size_t readBytes = readTotalBytes.back();
    uint64_t readTime = readTotalTime.back();
    readTotalBytes.pop_back();
    readTotalTime.pop_back();
    if (readBytes > 0) {
      esmftrc_default_trace_ioread(esmftrc_platform_get_default_ctx(),
                                    readBytes, readTime);
    }

    size_t writeBytes = writeTotalBytes.back();
    uint64_t writeTime = writeTotalTime.back();
    writeTotalBytes.pop_back();
    writeTotalTime.pop_back();
    if (writeBytes > 0) {
      esmftrc_default_trace_iowrite(esmftrc_platform_get_default_ctx(),
                                    writeBytes, writeTime);
    }

  }
  
  ////////////////////////////////////////////////////

  /////////////////// MPI /////////////////////

  static vector<size_t> mpiBarrierCount;
  static vector<uint64_t> mpiBarrierTime;
  static uint64_t mpiBarrierStartTimestamp = -1;
  
  void TraceMPIBarrierStart() {
    mpiBarrierStartTimestamp = TraceGetClock(NULL);
    mpiBarrierCount.back() = mpiBarrierCount.back() + 1;
  }

  void TraceMPIBarrierEnd() {
    uint64_t mpiBarrierEndTimestamp = TraceGetClock(NULL);
    mpiBarrierTime.back() = mpiBarrierTime.back() + (mpiBarrierEndTimestamp - mpiBarrierStartTimestamp);
    mpiBarrierStartTimestamp = -1;
  }

  static vector<size_t> mpiWaitCount;
  static vector<uint64_t> mpiWaitTime;
  static uint64_t mpiWaitStartTimestamp = -1;
  
  void TraceMPIWaitStart() {
    mpiWaitStartTimestamp = TraceGetClock(NULL);
    mpiWaitCount.back() = mpiWaitCount.back() + 1;
  }
  
  void TraceMPIWaitEnd() {
    uint64_t mpiWaitEndTimestamp = TraceGetClock(NULL);
    mpiWaitTime.back() = mpiWaitTime.back() + (mpiWaitEndTimestamp - mpiWaitStartTimestamp);
    mpiWaitStartTimestamp = -1;
  }  

  /*
   * This function used only in tests.
   */
  void TraceTest_GetMPIWaitStats(int *count, long long *time) {
    if (!traceInitialized) return;
    if (count != NULL) {
      *count = (int) mpiWaitCount.back();
    }
    if (time != NULL) {
      *time = (long long) mpiWaitTime.back();
    }
  }

  
  static void PushMPIStats() {
    mpiBarrierCount.push_back(0);
    mpiBarrierTime.push_back(0);
    mpiWaitCount.push_back(0);
    mpiWaitTime.push_back(0);
  }

  static void PopMPIStats() {

    size_t count = mpiBarrierCount.back();
    uint64_t time = mpiBarrierTime.back();
    mpiBarrierCount.pop_back();
    mpiBarrierTime.pop_back();
    if (count > 0) {
      esmftrc_default_trace_mpibarrier(esmftrc_platform_get_default_ctx(),
                                       count, time);
    }

    count = mpiWaitCount.back();
    time = mpiWaitTime.back();
    mpiWaitCount.pop_back();
    mpiWaitTime.pop_back();
    if (count > 0) {
      esmftrc_default_trace_mpiwait(esmftrc_platform_get_default_ctx(),
                                    count, time);
    }
    
  }

  
  /////////////////////////////////////////////
  
  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;

    PushIOStats();  /* reset statistics for this region */
    PushMPIStats();

    //TODO: combine regions below with MPI and IO stats above
    if (profileActive) {
      int regionId = -1;
      ESMFPhaseId phaseId(ESMFId(*ep_vmid, *ep_baseid), *ep_method, *ep_phase);
      bool present = phaseRegionMap.get(phaseId, regionId);
      if (!present) {
        regionId = next_region_id();
        phaseRegionMap.put(phaseId, regionId);
      }
      currentRegionNode = currentRegionNode->getOrAddChild(regionId);
      currentRegionNode->entered(TraceGetClock(NULL));
    }
    
    esmftrc_default_trace_phase_enter(esmftrc_platform_get_default_ctx(),
                                      *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }
  
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;

    PopIOStats();  /* issue events if there was I/O activity */
    PopMPIStats();

    if (profileActive) {
      /* assume phases are well-formed, so do not resolve regionId */
      currentRegionNode->exited(TraceGetClock(NULL));
      currentRegionNode = currentRegionNode->getParent();
    }
    
    esmftrc_default_trace_phase_exit(esmftrc_platform_get_default_ctx(),
                                     *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;
    esmftrc_default_trace_prologue_enter(esmftrc_platform_get_default_ctx(),
                                         *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }
  
  void TraceEventPhaseEpilogueExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;
    esmftrc_default_trace_epilogue_exit(esmftrc_platform_get_default_ctx(),
                                        *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventRegionEnter(std::string name) {
    if (!traceLocalPet) return;

    int regionId = 0;
    bool present = userRegionMap.get(name, regionId);
    if (!present) {
      regionId = next_region_id();
      userRegionMap.put(name, regionId);
      //add definition to trace
      esmftrc_default_trace_define_region(esmftrc_platform_get_default_ctx(),
                                          regionId,
                                          name.c_str());
    }

    if (profileActive) {
      currentRegionNode = currentRegionNode->getOrAddChild(regionId, true);
      currentRegionNode->entered(TraceGetClock(NULL));
    }
    
    PushIOStats();
    PushMPIStats();
    
    esmftrc_default_trace_regionid_enter(esmftrc_platform_get_default_ctx(),
                                         regionId);
        
  }

  void TraceEventRegionExit(std::string name) {
    if (!traceLocalPet) return;

    int regionId = 0;
    bool present = userRegionMap.get(name, regionId);
    if (!present) {
      //if timing regions are well-formed, then this should
      //never happen since the region would have already been
      //added - but we'll allow for poorly formed regions
      regionId = next_region_id();
      userRegionMap.put(name, regionId);
      //add definition to trace
      esmftrc_default_trace_define_region(esmftrc_platform_get_default_ctx(),
                                          regionId,
                                          name.c_str());
    }
    else if (profileActive) {
      currentRegionNode->exited(TraceGetClock(NULL));
      currentRegionNode = currentRegionNode->getParent();
    }

    PopIOStats();
    PopMPIStats();
    
    esmftrc_default_trace_regionid_exit(esmftrc_platform_get_default_ctx(),
                                         regionId);

  }

  //IPDv00p1=6||IPDv00p2=7||IPDv00p3=4||IPDv00p4=5::RunPhase1=1::FinalizePhase1=1
  //TODO: need a component level class (vmid, baseid) -> comp name?
  static void UpdateComponentInfoMap(string phaseStr, ESMFId esmfId, int method, string compName) {
    if (phaseStr.length() > 1) {
      vector<string> phases = split(trim(phaseStr), "||");
      for (unsigned i = 0; i < phases.size(); i++) {
        vector<string> phase = split(trim(phases.at(i)), "=");
        if (phase.size() == 2) {
          int phasenum = -1;
          stringstream ss(trim(phase.at(1)));
          ss >> phasenum;
          if(!(ss.fail())) {
            ComponentInfo *ci = NULL;
            bool present = componentInfoMap.get(esmfId, ci);
            if (!present) {
              //TODO: free this memory
              ci = new ComponentInfo(esmfId, compName);
              componentInfoMap.put(esmfId, ci);
            }
            ci->setPhaseName(ESMFPhaseId(esmfId, method, phasenum), phase.at(0)); 
            std::cout << "Added region: " + compName + ", " + phase.at(0) + "\n";
          }
        }
      }
    }   
  }
  
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventComponentInfo()"  
  void TraceEventComponentInfo(Comp *comp, int *ep_vmid, int *ep_baseid,
                               const char *ep_name, std::string attributeKeys,
                               std::string attributeVals) {

    if (!traceLocalPet) return;

    const vector<string> attrKeys = split(attributeKeys, "::");
    const vector<string> attrVals = split(attributeVals, "::");
    string ipm("");
    string rpm("");
    string fpm("");
    string iipm("");
    
    for (unsigned i=0; i < attrKeys.size(); i++) {
      if (attrKeys.at(i) == "IPM") {
        ipm = attrVals.at(i);
      }
      else if (attrKeys.at(i) == "RPM") {
        rpm = attrVals.at(i);
      }
      else if (attrKeys.at(i) == "FPM") {
        fpm = attrVals.at(i);
      }
      else if (attrKeys.at(i) == "IIPM") {
        iipm = attrVals.at(i);
      }
    }
    
    esmftrc_default_trace_comp(esmftrc_platform_get_default_ctx(),
                               *ep_vmid, *ep_baseid, ep_name,
                               ipm.c_str(), rpm.c_str(), fpm.c_str());

    if (profileActive) {
      string compName(ep_name);
      ESMFId esmfId(*ep_vmid, *ep_baseid);
      UpdateComponentInfoMap(ipm, esmfId, 0, compName);
      UpdateComponentInfoMap(iipm, esmfId, 0, compName);
      UpdateComponentInfoMap(rpm, esmfId, 1, compName);
      UpdateComponentInfoMap(fpm, esmfId, 2, compName);
    }
    
  }

  
#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventMemInfo()"
  void TraceEventMemInfo() {

    if (!traceLocalPet) return;
    
    int localrc;
    VM *globalvm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &localrc)) 
      return;
    
    int virtMem = -1;
    int physMem = -1;
    globalvm->getMemInfo(&virtMem, &physMem);    
    
    esmftrc_default_trace_mem(esmftrc_platform_get_default_ctx(),
                              virtMem, physMem);
    
  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventClock()"
  void TraceEventClock(int *ep_year, int *ep_month, int *ep_day,
                       int *ep_hour, int *ep_minute, int *ep_second) {
    
    if (!traceLocalPet) return;

    esmftrc_default_trace_clk(esmftrc_platform_get_default_ctx(),
                              *ep_year, *ep_month, *ep_day,
                              *ep_hour, *ep_minute, *ep_second);
    
  }

}

