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
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef ESMF_OS_MinGW
#include <unistd.h>
#else
#include <Winsock.h>
#endif

#ifdef ESMF_OS_Darwin
#include <mach/mach.h>
#include <mach/clock.h>
#include <mach/mach_time.h>
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
    unsigned long operator()(const string& s) const
    {
      unsigned long hash = 5381;
      int c;
      const char *str = s.c_str();     
      while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
      //printf("hash for %s = %d\n", s.c_str(), hash % REGION_HASHTABLE_SIZE);
      return hash % REGION_HASHTABLE_SIZE;
    }
  };

  static bool traceInitialized = false;
  static bool traceLocalPet = false;
  static int traceClock = 0;
  static int64_t traceClockOffset = 0;
  static HashMap<string, int, REGION_HASHTABLE_SIZE, StringHashF> regionMap;
  static int nextRegionId = 1;
  
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

  static string trim(const string& str) {
    size_t first = str.find_first_not_of(' ');
    if (string::npos == first) {
      return str;
    }
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
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

#ifdef ESMF_OS_MinGW
  struct timespec { long tv_sec; long tv_nsec; };
  static int unix_time(struct timespec *spec) {
	 __int64 wintime; GetSystemTimeAsFileTime((FILETIME*)&wintime);
     wintime      -=116444736000000000LL;       //1jan1601 to 1jan1970
     spec->tv_sec  =wintime / 10000000LL;       //seconds
     spec->tv_nsec =wintime % 10000000LL *100;  //nano-seconds
     return 0;
  }
  static int clock_gettime(int, timespec *spec) {
	 static  struct timespec startspec; static double ticks2nano;
     static __int64 startticks, tps =0;    __int64 tmp, curticks;
     QueryPerformanceFrequency((LARGE_INTEGER*)&tmp);
     if (tps !=tmp) { tps =tmp;
                      QueryPerformanceCounter((LARGE_INTEGER*)&startticks);
                      unix_time(&startspec); ticks2nano =(double)1000000000LL / tps; }
     QueryPerformanceCounter((LARGE_INTEGER*)&curticks); curticks -=startticks;
     spec->tv_sec  =startspec.tv_sec   +         (curticks / tps);
     spec->tv_nsec =startspec.tv_nsec  + (double)(curticks % tps) * ticks2nano;
     if (!(spec->tv_nsec < 1000000000LL)) { spec->tv_sec++; spec->tv_nsec -=1000000000LL; }
     return 0;
  }
#endif

  
  /* get current wallclock time */
  static uint64_t get_real_clock() {
    struct timespec ts;

#ifdef ESMF_OS_Darwin
    mach_timespec_t mts;
    static clock_serv_t rt_clock_serv = 0;

    if (rt_clock_serv == 0) {
      (void) host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &rt_clock_serv);
    }
    (void) clock_get_time(rt_clock_serv, &mts);
    ts.tv_sec = mts.tv_sec;
    ts.tv_nsec = mts.tv_nsec;
#elif ESMF_OS_MinGW
    clock_gettime(0, &ts);
#else
    clock_gettime(CLOCK_REALTIME, &ts);
#endif
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
  }

  
  /* get local monotonic time with no offset*/
  static uint64_t get_monotonic_raw_clock() {
    struct timespec ts;

#ifdef ESMF_OS_Darwin
    mach_timespec_t mts;
    static clock_serv_t rt_clock_serv = 0;

    if (rt_clock_serv == 0) {
      (void) host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &rt_clock_serv);
    }
    (void) clock_get_time(rt_clock_serv, &mts);
    ts.tv_sec = mts.tv_sec;
    ts.tv_nsec = mts.tv_nsec;
#elif ESMF_OS_MinGW
    clock_gettime(0, &ts);
#else
    clock_gettime(CLOCK_MONOTONIC, &ts);
#endif
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
  }

  /* get local monotonic time including local offset */
  static uint64_t get_monotonic_clock() {
    if (traceClockOffset < 0) {
      return get_monotonic_raw_clock() - (-1*traceClockOffset);
    }
    else {
      return get_monotonic_raw_clock() + traceClockOffset;
    }
  }
  
  
#define CLOCK_SYNC_TAG 42000
#define RTTMIN_NOTCHANGED_MAX 100
  
  static int64_t clock_measure_offset(VM *vm, int peerPet, int64_t root_offset) {

    int myPet = vm->getLocalPet();
    //printf("[%d] entered clock_measure_offset\n", myPet);

    uint64_t starttime, stoptime, peertime;
    uint64_t rtt, rttmin, invalidtime;
    int rttmin_notchanged;
    int64_t offset;
    invalidtime = 42000000000ULL;
    rttmin = 1E12;
    offset = 0;
    rttmin_notchanged = 0;
    
    for (;;) {

      if (myPet != 0) {
        starttime = get_monotonic_raw_clock();
        //printf("[%d] about to send starttime: %ldd\n", myPet, starttime);
        vm->send(&starttime, sizeof(uint64_t), 0, CLOCK_SYNC_TAG);
        vm->recv(&peertime, sizeof(uint64_t), 0, CLOCK_SYNC_TAG);
        stoptime = get_monotonic_raw_clock();
        rtt = stoptime - starttime;

        //printf("[%d] rtt = %ldd\n", myPet, rtt);
        
        if (rtt < rttmin) {
          rttmin = rtt;
          rttmin_notchanged = 0;
          offset = peertime - (rtt / 2) - starttime;
        }
        else if (++rttmin_notchanged == RTTMIN_NOTCHANGED_MAX) {
          vm->send(&invalidtime, sizeof(uint64_t), 0, CLOCK_SYNC_TAG);
          break;
        }
      }
      else {  /* root PET */
        vm->recv(&starttime, sizeof(uint64_t), peerPet, CLOCK_SYNC_TAG);
        //printf("[%d] received starttime: %ldd\n", peerPet, starttime);
        peertime = get_monotonic_raw_clock() + root_offset;
        if (starttime == invalidtime) {
          break;
        }
        vm->send(&peertime, sizeof(uint64_t), peerPet, CLOCK_SYNC_TAG);
        //printf("[%d] sent peertime to PET %d = %ldd\n", myPet, peerPet, peertime);
      }

    }

    //if( myPet != 0 ){
    //  *min_rtt = rttmin;
    //} else {
    //  rtt = 0.0;
    // }
    return offset;    
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::clock_sync_offset()"  
  static int64_t clock_sync_offset(int *rc) {

    int localrc;
    if (rc != NULL) *rc = ESMF_SUCCESS;
    
    VM *vm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
      return 0;

    int myPet = vm->getLocalPet();
    int petCount = vm->getPetCount();
    int64_t ret;
    
    for (int peer = 1; peer < petCount; peer++) {
      vm->barrier();
      if (myPet == 0 || myPet == peer) {
        //printf("clock_measure_offset between root and peer: %d\n", peer);
        ret = clock_measure_offset(vm, peer, 0);
      }
    }
    return ret;

  }

  static uint64_t get_clock(void *data) {
    switch(traceClock) {
    case ESMF_CLOCK_REALTIME:
      return get_real_clock();
    case ESMF_CLOCK_MONOTONIC:
      return get_monotonic_raw_clock();
    case ESMF_CLOCK_MONOTONIC_SYNC:
      return get_monotonic_clock();      
    }
    return 0;
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
        
    if (rc != NULL) *rc = ESMF_SUCCESS;

    VM *globalvm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, 
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) 
      return;
    int stream_id = globalvm->getLocalPet();
        
    //determine which clock to use
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
        
    // set up callbacks
    cbs.sys_clock_clock_get_value = get_clock;
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
    }
    
  }

  ///////////////////// I/O Tracing //////////////////

  static std::string openFilename;
  static uint64_t openStartTimestamp = -1;

  void TraceIOOpenStart(const char *path) {
    if (!traceLocalPet) return;    
    openStartTimestamp = get_clock(NULL);
    openFilename = string(path);
  }
  
  void TraceIOOpenEnd() {
    if (!traceLocalPet) return;
    uint64_t openEndTimestamp = get_clock(NULL);
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
    writeStartTimestamp = get_clock(NULL);
  }

  void TraceIOWriteEnd(size_t nbytes) {
    uint64_t writeEndTimestamp = get_clock(NULL);
    writeTotalTime.back() = writeTotalTime.back() + (writeEndTimestamp - writeStartTimestamp);
    writeTotalBytes.back() = writeTotalBytes.back() + nbytes;
    writeStartTimestamp = -1;
  }

  static vector<size_t> readTotalBytes;
  static vector<uint64_t> readTotalTime;
  static uint64_t readStartTimestamp = -1;
  
  void TraceIOReadStart() {
    readStartTimestamp = get_clock(NULL);
  }

  void TraceIOReadEnd(size_t nbytes) {
    uint64_t readEndTimestamp = get_clock(NULL);
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
    mpiBarrierStartTimestamp = get_clock(NULL);
    mpiBarrierCount.back() = mpiBarrierCount.back() + 1;
  }

  void TraceMPIBarrierEnd() {
    uint64_t mpiBarrierEndTimestamp = get_clock(NULL);
    mpiBarrierTime.back() = mpiBarrierTime.back() + (mpiBarrierEndTimestamp - mpiBarrierStartTimestamp);
    mpiBarrierStartTimestamp = -1;
  }

  static vector<size_t> mpiWaitCount;
  static vector<uint64_t> mpiWaitTime;
  static uint64_t mpiWaitStartTimestamp = -1;
  
  void TraceMPIWaitStart() {
    mpiWaitStartTimestamp = get_clock(NULL);
    mpiWaitCount.back() = mpiWaitCount.back() + 1;
  }
  
  void TraceMPIWaitEnd() {
    uint64_t mpiWaitEndTimestamp = get_clock(NULL);
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
    
    esmftrc_default_trace_phase_enter(esmftrc_platform_get_default_ctx(),
                                      *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }
  
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;

    PopIOStats();  /* issue events if there was I/O activity */
    PopMPIStats();
    
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

    int region_id = 0;
    bool present = regionMap.get(name, region_id);
    if (!present) {
      region_id = nextRegionId;
      nextRegionId++;
      regionMap.put(name, region_id);
      //add definition to trace
      esmftrc_default_trace_define_region(esmftrc_platform_get_default_ctx(),
                                          region_id,
                                          name.c_str());
    }

    PushIOStats();
    PushMPIStats();
    
    esmftrc_default_trace_regionid_enter(esmftrc_platform_get_default_ctx(),
                                         region_id);
        
  }

  void TraceEventRegionExit(std::string name) {
    if (!traceLocalPet) return;

    int region_id = 0;
    bool present = regionMap.get(name, region_id);
    if (!present) {
      //if timing regions are well-formed, then this should
      //never happen since the region would have already been
      //added - but we'll allow for poorly formed regions
      region_id = nextRegionId;
      nextRegionId++;
      regionMap.put(name, region_id);
      //add definition to trace
      esmftrc_default_trace_define_region(esmftrc_platform_get_default_ctx(),
                                          region_id,
                                          name.c_str());
    }

    PopIOStats();
    PopMPIStats();
    
    esmftrc_default_trace_regionid_exit(esmftrc_platform_get_default_ctx(),
                                         region_id);

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
    }
    
    esmftrc_default_trace_comp(esmftrc_platform_get_default_ctx(),
                               *ep_vmid, *ep_baseid, ep_name,
                               ipm.c_str(), rpm.c_str(), fpm.c_str());
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

