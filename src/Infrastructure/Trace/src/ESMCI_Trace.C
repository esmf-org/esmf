// $Id$
/*
 * Writes trace events to the file system.
 *
 * Earth System Modeling Framework
 * Copyright 2002-2021, University Corporation for Atmospheric Research,
 * Massachusetts Institute of Technology, Geophysical Fluid Dynamics
 * Laboratory, University of Michigan, National Centers for Environmental
 * Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
 * NASA Goddard Space Flight Center.
 * Licensed under the University of Illinois-NCSA License.
 */

#include <iomanip>
#include <sstream>
#include <string>
#include <algorithm>
#include <map>

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
#include "ESMCI_VMKernel.h"
#include "ESMCI_HashMap.h"
#include "ESMCI_RegionNode.h"
#include "ESMCI_RegionSummary.h"
#include "ESMCI_ComponentInfo.h"
#include "ESMCI_TraceUtil.h"
#include "ESMCI_Comp.h"
#include <esmftrc.h>

#ifndef ESMF_OS_MinGW
#define TRACE_DIR_PERMISSIONS (S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH)
#else
#define TRACE_DIR_PERMISSIONS (S_IRWXU)
#endif

#define EVENT_BUF_SIZE_DEFAULT 4096
#define EVENT_BUF_SIZE_EAGER 1024
#define REGION_HASHTABLE_SIZE 100
#define VMID_MAP_SIZE 10000

using std::string;
using std::vector;
using std::stringstream;
using std::ofstream;
using std::map;

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

  static bool traceInitialized = false;  // is trace ready for events?
  static bool traceLocalPet = false;     // is tracing on for this PET?
  static bool profileLocalPet = false;   // is profiling on for this PET?
  static bool profileOutputToLog = false;    // output to EMSF log?
  static bool profileOutputToFile = false;   // output to text file?
  static bool profileOutputToBinary = false; // output to binary trace?
  static bool profileOutputSummary = false;   // output aggregate profile on root PET?

  static uint16_t next_local_id() {
    static uint16_t next = 1;
    if (next > REGION_MAX_COUNT) {
      throw std::range_error("Out of space for trace regions");
    }
    return next++;
  }

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

  static HashMap<string, uint16_t, REGION_HASHTABLE_SIZE, StringHashF> userRegionMap;
  static HashMap<ESMFPhaseId, uint16_t, REGION_HASHTABLE_SIZE, ESMFPhaseHashF> phaseRegionMap;
  static HashMap<ESMFId, ComponentInfo *, REGION_HASHTABLE_SIZE, ESMFIdHashF> componentInfoMap;

  static RegionNode rootRegionNode(NULL, next_local_id(), false);
  static RegionNode *currentRegionNode = &rootRegionNode;

#ifndef ESMF_NO_DLFCN
  static int (*notify_wrappers)(int initialized) = NULL;
#endif

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceMapVmId()"
  int TraceMapVmId(VMId *vmid, int *rc) {

    //this data structure used to map VMIds(vmKey,localid)
    //to an integer id
    static VMId vmIdMap[VMID_MAP_SIZE];
    static int nextVmId = 0;

    int localrc = ESMC_RC_NOT_IMPL;
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    int foundIdx;
    //search backward - vm more likely to be at the end
    for (foundIdx=nextVmId-1; foundIdx >= 0; foundIdx--) {
      if (VMIdCompare(vmid, &(vmIdMap[foundIdx]))) {
        if (rc!=NULL) *rc = ESMF_SUCCESS;
        return foundIdx;
      }
    }
    if (nextVmId >= VMID_MAP_SIZE) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                    "Out of VmIdMap space inside tracing", ESMC_CONTEXT, rc);
      return -1;
    }
    else {
      localrc = (&vmIdMap[nextVmId])->create();
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

  bool TraceInitialized() {
    return traceInitialized;
  }


#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::CheckPETList"
  static bool CheckPETList(string petList, int petToCheck, int *rc) {
    if (rc!=NULL) *rc = ESMF_SUCCESS;

    bool invalidFormat = false;
    const vector<string> listItems = split(trim(petList), " ");

    for (unsigned i = 0; i < listItems.size(); i++) {
      if (listItems.at(i).find("-") != string::npos) {
        vector<string> petRange = split(trim(listItems.at(i)), "-");
        if (petRange.size() == 2) {
          int low, high;
          stringstream lowss(trim(petRange.at(0)));
          stringstream highss(trim(petRange.at(1)));
          if (!(lowss >> low).fail() && !(highss >> high).fail()) {
            //printf("low=%d, high=%d\n", low, high);
            if (petToCheck >= low && petToCheck <= high) {
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
          if (petToCheck == pet) {
            return true;
          }
        }
        else {
          invalidFormat = true;
        }
      }
      if (invalidFormat) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                                      "Invalid PET list format.",
                                      ESMC_CONTEXT, rc);
        return false;
      }

    }

    //pet not found in list
    return false;
}


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::GlobalProfileOptions()"
  static void GlobalProfileOptions(int *traceIsOn, int *profileIsOn, int *rc) {
    *rc = ESMC_RC_NOT_IMPL;
    if (traceIsOn != NULL) {
      *traceIsOn = 0;
      char const *envVar = VM::getenv("ESMF_RUNTIME_TRACE");
      if (envVar != NULL) {
        std::string value(envVar);
        size_t index;
        index = value.find("on");
        if (index == std::string::npos)
          index = value.find("ON");
        if (index != std::string::npos){
          *traceIsOn=1;
        }
      }
    }
    if (profileIsOn != NULL) {
      *profileIsOn = 0;
      char const *envVar = VM::getenv("ESMF_RUNTIME_PROFILE");
      if (envVar != NULL) {
        std::string value(envVar);
        size_t index;
        index = value.find("on");
        if (index == std::string::npos)
          index = value.find("ON");
        if (index != std::string::npos){
          *profileIsOn=1;
        }
      }
    }
    *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::ProfileIsEnabledForPET()"
  static bool ProfileIsEnabledForPET(int petToCheck, int *rc) {
    int localrc;
    if (rc != NULL) *rc = ESMF_SUCCESS;

    int tracingEnabled = 0;
    int profilingEnabled = 0;
    GlobalProfileOptions(&tracingEnabled, &profilingEnabled, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
      return false;

    if (profilingEnabled == 0) return false;

    //always profile PET 0?
    if (petToCheck == 0) return true;

    char const *envVar = VM::getenv("ESMF_RUNTIME_PROFILE_PETLIST");
    if (envVar != NULL) {
      string envStr(envVar);
      bool inPetList = CheckPETList(envStr, petToCheck, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,
           "Invalid format in ESMF_RUNTIME_PROFILE_PETLIST environment variable.", ESMC_CONTEXT, rc))
        return false;
      return inPetList;
    }
    else {
      //default to profile all PETs
      return true;
    }
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceIsEnabledForPET()"
  static bool TraceIsEnabledForPET(int petToCheck, int *rc){
    int localrc;
    if (rc != NULL) *rc = ESMF_SUCCESS;

    //first check if tracing is enabled
    int tracingEnabled = 0;
    int profilingEnabled = 0;
    GlobalProfileOptions(&tracingEnabled, &profilingEnabled, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
      return false;

    if (tracingEnabled == 0) return false;

    //always trace PET 0
    if (petToCheck == 0) return true;

    char const *envVar = VM::getenv("ESMF_RUNTIME_TRACE_PETLIST");
    if (envVar != NULL) {
      string envStr(envVar);
      bool inPetList = CheckPETList(envStr, petToCheck, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,
           "Invalid format in ESMF_RUNTIME_TRACE_PETLIST environment variable.", ESMC_CONTEXT, rc))
        return false;
      return inPetList;
    }
    else {
      //default to trace all PETs
      return true;
    }

  }

  //global context
  static struct esmftrc_platform_filesys_ctx *traceCtx = NULL;

  static struct esmftrc_default_ctx *esmftrc_platform_get_default_ctx() {
    return &traceCtx->ctx;
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

    if (rc!=NULL) *rc = ESMF_SUCCESS;

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

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::InitializeWrappers()"
  static void InitializeWrappers() {
    int wrappersPresent = TRACE_WRAP_NONE;
#ifndef ESMF_NO_DLFCN
    void *preload_lib = dlopen(NULL, RTLD_LAZY);
    if (preload_lib == NULL) {
      ESMC_LogDefault.Write("ESMF Tracing/Profiling could not open shared library containing instrumentation.", ESMC_LOGMSG_WARN);
    }
    else {
      notify_wrappers = (int (*)(int)) dlsym(preload_lib, "c_esmftrace_notify_wrappers");
      if (notify_wrappers != NULL) {
        wrappersPresent = notify_wrappers(1);
      }
      else {
        ESMC_LogDefault.Write("ESMF Tracing/Profiling could not load dynamic instrumentation functions.", ESMC_LOGMSG_WARN);
      }
    }
#else
    wrappersPresent = c_esmftrace_notify_wrappers(1);
#endif

    if (wrappersPresent != TRACE_WRAP_NONE) {
      stringstream logMsg;
      logMsg << "ESMF Tracing/Profiling enabled with ";
      if (wrappersPresent == TRACE_WRAP_DYNAMIC) {
        logMsg << "DYNAMIC";
      }
      else if (wrappersPresent == TRACE_WRAP_STATIC) {
        logMsg << "STATIC";
      }
      logMsg << " instrumentation.";
      ESMC_LogDefault.Write(logMsg.str().c_str(), ESMC_LOGMSG_INFO);
      logMsg.str("  This option should only be used for profiling applications and NOT for production runs.");
      ESMC_LogDefault.Write(logMsg.str().c_str(), ESMC_LOGMSG_INFO);
    }
  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::FinalizeWrappers()"
  static void FinalizeWrappers() {
#ifndef ESMF_NO_DLFCN
    if (notify_wrappers != NULL) {
      notify_wrappers(0);
    }
#else
    c_esmftrace_notify_wrappers(0);
#endif
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceOpen()"
  void TraceOpen(std::string trace_dir, int *profileToLog, int *rc) {

    int localrc;
    stringstream logMsg;

    if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;

    VM *globalvm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
      return;

    //determine if tracing is turned on for this PET
    traceLocalPet = TraceIsEnabledForPET(globalvm->getLocalPet(), &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
      traceLocalPet = false;
      return;
    }

    //determine if profiling is turned on for this PET
    //if tracing is enabled, automatically turn on profiling
    profileLocalPet = traceLocalPet || ProfileIsEnabledForPET(globalvm->getLocalPet(), &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
         ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
      profileLocalPet = false;
      return;
    }

    //determine output method for profiling, if enabled
    if (profileLocalPet) {
      //always output binary if tracing is enabled
      if (traceLocalPet) profileOutputToBinary = true;
      char const *envProfileOutput = VM::getenv("ESMF_RUNTIME_PROFILE_OUTPUT");
      if (envProfileOutput != NULL && strlen(envProfileOutput) > 0) {
        string profileOutput(envProfileOutput);
        if ( (profileOutput.find("TEXT") != string::npos) ||
             (profileOutput.find("text") != string::npos) ||
             (profileOutput.find("Text") != string::npos) ) {
          if (profileToLog != NULL && *profileToLog == 1) {
            profileOutputToLog = true;
          }
          else {
            profileOutputToFile = true;
          }
        }
        if ( (profileOutput.find("BINARY") != string::npos) ||
             (profileOutput.find("binary") != string::npos) ||
             (profileOutput.find("Binary") != string::npos) ) {
          profileOutputToBinary = true;
        }
        if ( (profileOutput.find("SUMMARY") != string::npos) ||
             (profileOutput.find("summary") != string::npos) ||
             (profileOutput.find("Summary") != string::npos) ) {
          profileOutputSummary = true;
        }
      }
      else {
        // if not specified, default is to output text
        if (profileToLog != NULL && *profileToLog == 1) {
          //printf("set output to log\n");
          profileOutputToLog = true;
        }
        else {
          //printf("set output to file\n");
          profileOutputToFile = true;
        }
      }
    }

    if (traceLocalPet) {
      ESMC_LogDefault.Write("ESMF Tracing Enabled", ESMC_LOGMSG_INFO);
    }
    if (profileLocalPet) {
      ESMC_LogDefault.Write("ESMF Profiling Enabled", ESMC_LOGMSG_INFO);
    }

    // initialize the clock
    struct esmftrc_platform_filesys_ctx *ctx;
    if (traceLocalPet || profileLocalPet) {
      ctx = FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, malloc(sizeof(*ctx)));
      if (!ctx) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate context",
                                      ESMC_CONTEXT, rc);
        return;
      }
      ctx->latch_ts = 0;
      ctx->fh == NULL;

      //store as global context
      traceCtx = ctx;

      TraceInitializeClock(&localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,
           ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
        return;
    }

    // determine if we need to set up for binary output
    if (traceLocalPet || profileOutputToBinary) {

      // stream_id same as global pet id
      int stream_id = globalvm->getLocalPet();
      ctx->stream_id = stream_id;
      //get node name
      if (gethostname(ctx->nodename, NODENAME_LEN) < 0) {
        ctx->nodename[0] = '\0';
      }

      // set up callbacks
      struct esmftrc_platform_callbacks cbs;
      cbs.sys_clock_clock_get_value = TraceGetClock;
      cbs.is_backend_full = is_backend_full;
      cbs.open_packet = open_packet;
      cbs.close_packet = close_packet;

      //allocate event buffer
      char const *envFlush = VM::getenv("ESMF_RUNTIME_TRACE_FLUSH");
      string strFlush = "DEFAULT";
      int eventBufSize = EVENT_BUF_SIZE_DEFAULT;
      if (envFlush != NULL) {
        strFlush = envFlush;
        if (trim(strFlush) == "EAGER" || trim(strFlush) == "eager" || trim(strFlush) == "Eager") {
          eventBufSize = EVENT_BUF_SIZE_EAGER;
          logMsg.str("ESMF Tracing set to EAGER flushing.");
          ESMC_LogDefault.Write(logMsg.str().c_str(), ESMC_LOGMSG_INFO);
        }
      }

      uint8_t *buf = FROM_VOID_PTR(uint8_t, malloc(eventBufSize));
      if (!buf) {
        free(ctx);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE, "Cannot allocate trace event buffer",
                                      ESMC_CONTEXT, rc);
        return;
      }
      memset(buf, 0, eventBufSize);

      //make relative path absolute if needed
      string stream_dir_root;
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

      // all PETs wait for directory to be created
      globalvm->barrier();

      // my specific file
      stringstream stream_file;
      stream_file << stream_dir_root << "/esmf_stream_" << std::setfill('0') << std::setw(4) << stream_id;

      ctx->fh = fopen(stream_file.str().c_str(), "wb");
      if (!ctx->fh) {
        free(ctx);
        free(buf);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_OPEN, "Error opening trace output file",
                                      ESMC_CONTEXT, rc);
        return;
      }

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

    }
    else {
      // this PET either has no tracing/profiling or only profiling to log/text
      globalvm->barrier();  //match barrier call above
    }

    if (traceLocalPet || profileLocalPet) {
      traceInitialized = true;
      // notify any function wrappers that trace is ready
      InitializeWrappers();
    }

    if (rc!=NULL) *rc = ESMF_SUCCESS;

  }

  static string getPhaseNameFromPhaseId(ESMFPhaseId phaseId) {
    ComponentInfo *ci = NULL;
    bool present = componentInfoMap.get(phaseId.getESMFId(), ci);
    if (present && ci != NULL) {
      return ci->getPhaseName(phaseId);
    }
    return "";
  }

  static string getRegionNameFromId(uint16_t local_id) {
    ESMFPhaseId phaseId;
    bool present = phaseRegionMap.reverse(local_id, phaseId);
    if (present) {
      return getPhaseNameFromPhaseId(phaseId);
    }
    else {
      string name;
      present = userRegionMap.reverse(local_id, name);
      if (present) return name;
    }
    return "";
  }


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::populateRegionNames()"
  static void populateRegionNames(RegionNode *rn) {
    if (rn == NULL) return;

    string name = getRegionNameFromId(rn->getLocalId());
    if (name.length() == 0) {
      if (rn->isUserRegion()) {
        name = "UNKNOWN_USER_REGION";
      }
      else {
        name = "UNKNOWN_ESMF_PHASE";
      }
    }
    rn->setName(name);

    for (unsigned i = 0; i < rn->getChildren().size(); i++) {
      populateRegionNames(rn->getChildren().at(i));
    }
  }

  static size_t regionNamePadding(RegionSummary *rs, int depth) {
    size_t maxSize = rs->getName().length() + (2*depth);
    for (unsigned i = 0; i < rs->getChildren().size(); i++) {
      size_t childSize = regionNamePadding(rs->getChildren().at(i), depth+1);
      if (childSize > maxSize) maxSize = childSize;
    }
    return maxSize;
  }

  static size_t regionNamePadding(RegionNode *rn, int depth) {
    size_t maxSize = rn->getName().length() + (2*depth);
    for (unsigned i = 0; i < rn->getChildren().size(); i++) {
      size_t childSize = regionNamePadding(rn->getChildren().at(i), depth+1);
      if (childSize > maxSize) maxSize = childSize;
    }
    return maxSize;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::printProfile()"
#define STATLINE 512
  static void printProfile(RegionNode *rn, bool printToLog, string prefix, ofstream &ofs, size_t namePadding, int *rc) {

    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    if (rn->getParent() != NULL) {
      char strbuf[STATLINE];
      string name = rn->getName();
      name.insert(0, prefix);

      stringstream fmt;
      fmt << "%-" << namePadding << "s %-6lu %-11.4f %-11.4f %-11.4f %-11.4f %-11.4f";

      snprintf(strbuf, STATLINE, fmt.str().c_str(),
               name.c_str(), rn->getCount(), rn->getTotal()*NANOS_TO_SECS,
               rn->getSelfTime()*NANOS_TO_SECS, rn->getMean()*NANOS_TO_SECS,
               rn->getMin()*NANOS_TO_SECS, rn->getMax()*NANOS_TO_SECS);
      if (printToLog) {
        ESMC_LogDefault.Write(strbuf, ESMC_LOGMSG_INFO);
      }
      else {
        ofs << strbuf << "\n";
      }
    }
    rn->sortChildren();
    for (unsigned i = 0; i < rn->getChildren().size(); i++) {
      printProfile(rn->getChildren().at(i), printToLog, prefix + "  ", ofs, namePadding, rc);
    }
    if (rc!=NULL) *rc=ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::printProfile()"
  static void printProfile(RegionNode *rn, bool printToLog, string filename, int *rc) {

    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    ofstream ofs;
    int localrc;

    size_t namePadding = regionNamePadding(rn, 0)+1;
    if (namePadding > 200) namePadding = 200;

    stringstream fmt;
    fmt << "%-" << namePadding << "s %-6s %-11s %-11s %-11s %-11s %-11s";

    char strbuf[STATLINE];
    snprintf(strbuf, STATLINE, fmt.str().c_str(),
             "Region", "Count", "Total (s)", "Self (s)", "Mean (s)", "Min (s)", "Max (s)");

    if (printToLog) {
      ESMC_LogDefault.Write("**************** Region Timings *******************", ESMC_LOGMSG_INFO);
      ESMC_LogDefault.Write(strbuf, ESMC_LOGMSG_INFO);
    }
    else {
      ofs.open(filename.c_str(), ofstream::trunc);
      if (ofs.is_open() && !ofs.fail()) {
        ofs << strbuf << "\n";
      }
      else {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error opening profile output file",
           ESMC_CONTEXT, rc);
        return;
      }
    }
    printProfile(rn, printToLog, "", ofs, namePadding, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, "Error writing profile output file",
         ESMC_CONTEXT, rc))
      return;
    if (!printToLog) {
      ofs.close();
    }
    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::findImbalancedConnectors()"
  static void findImbalancedConnectors(RegionSummary *rs, vector<string> &connList, int *rc) {

    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    rs->sortChildren();
    for (unsigned i = 0; i < rs->getChildren().size(); i++) {
      //TODO: find a cleaner way of determining the type of component
      RegionSummary *child = rs->getChildren().at(i);
      if (child->getName().find("-TO-") != string::npos &&
	  child->getName().find("Run") != string::npos) {
	//only report if normalized time shows > 5% imbalance
	if (child->getParent()->getTotalMax() > 0) {
	  double ndiff = (1.0*child->getTotalMax() / child->getParent()->getTotalMax()) -
	    (1.0*child->getTotalMin() / child->getParent()->getTotalMax());
	  if (ndiff > .05) {
	    connList.push_back(child->getName());
	  }
	}
      }
    }

    for (unsigned i = 0; i < rs->getChildren().size(); i++) {
      findImbalancedConnectors(rs->getChildren().at(i), connList, rc);
    }

    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::printSummaryProfileMessage()"
  static void printSummaryProfileMessage(RegionSummary *rs, ofstream &ofs, int *rc) {

    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;
    int localrc;
    vector<string> connList;

    findImbalancedConnectors(rs, connList, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
	  ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
      return;

    if (connList.size() > 0) {
      string msg = "********";
      msg += "\nIMPORTANT: Large deviations between Connector times on different PETs\n";
      msg += "are typically indicators of load imbalance in the system. The following\n";
      msg += "Connectors in this profile may indicate a load imbalance:\n";
      for (unsigned i = 0; i < connList.size(); i++) {
	msg += "\t - " + connList.at(i) + "\n";
      }
      ofs << msg << "********\n\n";
    }

    if (rc!=NULL) *rc = ESMF_SUCCESS;

  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::printSummaryProfile()"
  static void printSummaryProfile(RegionSummary *rs, string prefix, ofstream &ofs, size_t namePadding, int *rc) {

    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    if (rs->getParent() != NULL) {
      char strbuf[STATLINE];
      string name = rs->getName();
      name.insert(0, prefix);

      char countstr[12];
      if (rs->getCountsMatch()) {
	snprintf(countstr, 12, "%-6lu", rs->getCountEach());
      }
      else {
	snprintf(countstr, 12, "%-8s", "MULTIPLE");
      }

      stringstream fmt;
      fmt << "%-" << namePadding << "s %-6lu %-8s %-11.4f %-11.4f %-7d %-11.4f %-7d";

      snprintf(strbuf, STATLINE, fmt.str().c_str(),
               name.c_str(), rs->getPetCount(), countstr,
	       rs->getTotalMean()*NANOS_TO_SECS,
	       rs->getTotalMin()*NANOS_TO_SECS, rs->getTotalMinPet(),
	       rs->getTotalMax()*NANOS_TO_SECS, rs->getTotalMaxPet());
      ofs << strbuf << "\n";
    }
    rs->sortChildren();
    for (unsigned i = 0; i < rs->getChildren().size(); i++) {
      printSummaryProfile(rs->getChildren().at(i), prefix + "  ", ofs, namePadding, rc);
    }
    if (rc!=NULL) *rc=ESMF_SUCCESS;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::printSummaryProfile()"
  static void printSummaryProfile(RegionSummary *rs, string filename, int *rc) {

    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    ofstream ofs;
    int localrc;

    size_t namePadding = regionNamePadding(rs, 0)+1;
    if (namePadding > 200) namePadding = 200;

    stringstream fmt;
    fmt << "%-" << namePadding << "s %-6s %-8s %-11s %-11s %-7s %-11s %-7s";

    char strbuf[STATLINE];
    snprintf(strbuf, STATLINE, fmt.str().c_str(),
             "Region", "PETs", "Count", "Mean (s)", "Min (s)", "Min PET", "Max (s)", "Max PET");

    ofs.open(filename.c_str(), ofstream::trunc);
    if (ofs.is_open() && !ofs.fail()) {
      printSummaryProfileMessage(rs, ofs, &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc, "Error writing profile footer",
	   ESMC_CONTEXT, rc))
	return;
      ofs << strbuf << "\n";
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error opening profile output file",
				    ESMC_CONTEXT, rc);
      return;
    }

    printSummaryProfile(rs, "", ofs, namePadding, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, "Error writing profile output file",
         ESMC_CONTEXT, rc))
      return;
    ofs.close();

    if (rc!=NULL) *rc = ESMF_SUCCESS;
  }



  static void AddRegionProfilesToTrace(RegionNode *rn) {

    esmftrc_default_trace_region_profile(
        esmftrc_platform_get_default_ctx(),
	rn->getGlobalId(),
	rn->getParentGlobalId(),
	rn->getTotal(),
	rn->getSelfTime(),
	rn->getCount(),
	rn->getMax(),
	rn->getMin(),
	rn->getMean(),
	rn->getStdDev());

    for (unsigned i = 0; i < rn->getChildren().size(); i++) {
      AddRegionProfilesToTrace(rn->getChildren().at(i));
    }
  }


#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::GatherRegions()"
  static void GatherRegions(int *rc) {

    int localrc;
    VM *globalvm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
      return;

    char *serializedTree = NULL;
    size_t bufferSize = 0;

    if (profileLocalPet && globalvm->getLocalPet() > 0) {
      //std::cout << "serialize from pet: " << globalvm->getLocalPet() << "\n";
      try {
        serializedTree = rootRegionNode.serialize(&bufferSize);
      }
      catch(std::exception& e) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                      e.what(), ESMC_CONTEXT, rc);
        return;
      }
      //std::cout << "sending profile from pet: " << globalvm->getLocalPet() << " (" << bufferSize << ")" << "\n";
      //send size of buffer
      globalvm->send((void *) &bufferSize, sizeof(bufferSize), 0);
      //send buffer itself
      globalvm->send((void *) serializedTree, bufferSize, 0);

      free(serializedTree);
    }
    else if (globalvm->getLocalPet() == 0) {

      //clone root
      //ESMCI::RegionNode *aggNode = new ESMCI::RegionNode(NULL, &rootRegionNode);
      ESMCI::RegionSummary *sumNode = new ESMCI::RegionSummary(NULL);

      //first add my own timing tree to the summary
      sumNode->merge(rootRegionNode, globalvm->getLocalPet());

      //then gather from other PETs
      for (int p=1; p<globalvm->getPetCount(); p++) {

        if (ProfileIsEnabledForPET(p, &localrc) || TraceIsEnabledForPET(p, &localrc)) {

          bufferSize = 0;
          globalvm->recv((void *) &bufferSize, sizeof(bufferSize), p);
          //std::cout << "receive profile from pet: " << p << " (" << bufferSize << ")" << "\n";

          serializedTree = (char *) malloc(bufferSize);
          if (serializedTree == NULL) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_MEM_ALLOCATE,
                                        "Error allocating memory when gather profiled regions",
                                        ESMC_CONTEXT, rc);
          return;
          }
          memset(serializedTree, 0, bufferSize);

          globalvm->recv(serializedTree, bufferSize, p);


          try {
	    ESMCI::RegionNode *desNode = new ESMCI::RegionNode(serializedTree, bufferSize);
	    //merge statistics
	    sumNode->merge(*desNode, p);
	    delete desNode;
	  }
          catch(std::exception& e) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
                                          e.what(), ESMC_CONTEXT, rc);
            return;
          }

          free(serializedTree);
        }
        else if (ESMC_LogDefault.MsgFoundError(localrc,
                   ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) {
          return;
        }

      }

      //now we have received and merged
      //profiles from all other PETs
      printSummaryProfile(sumNode, "ESMF_Profile.summary", &localrc);
      if (ESMC_LogDefault.MsgFoundError(localrc,
           ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
        return;

      delete sumNode;
    }

  }



#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceClose()"
  void TraceClose(int *rc) {

    int localrc;
    if (rc!=NULL) *rc = ESMC_RC_NOT_IMPL;

    // allow calling multiple times, only closes
    // on the first call, needed in testing
    if (traceInitialized) {
      traceInitialized = false;
      FinalizeWrappers();

      if (profileOutputToLog || profileOutputToFile || profileOutputSummary) {
        populateRegionNames(&rootRegionNode);
      }

      if (profileOutputToLog) {
        printProfile(&rootRegionNode, true, "", &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
             ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
          return;
      }

      if (profileOutputToFile) {
        VM *globalvm = VM::getGlobal(&localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
             ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
          return;

        stringstream fname;
        fname << (globalvm->getPetCount() - 1);
        int width = fname.str().length();
        fname.str("");
        fname << "ESMF_Profile." << std::setfill('0') << std::setw(width) << globalvm->getLocalPet();

        printProfile(&rootRegionNode, false, fname.str(), &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
             ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
          return;
      }

      if (profileOutputToBinary) {
        AddRegionProfilesToTrace(&rootRegionNode);
      }

      if (profileOutputSummary) {
        GatherRegions(&localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc,
           ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
          return;
      }

      if (traceCtx != NULL) {
        if (traceLocalPet || profileOutputToBinary) {
          if (traceCtx->fh != NULL) {
            if (esmftrc_packet_is_open(&traceCtx->ctx) &&
                !esmftrc_packet_is_empty(&traceCtx->ctx)) {
              close_packet(traceCtx);
            }
            fclose(traceCtx->fh);
          }
          free(esmftrc_packet_buf(&traceCtx->ctx));
        }
        free(traceCtx);
        traceCtx = NULL;
      }

      vector<HashNode<ESMFId, ComponentInfo *> *> entries = componentInfoMap.getEntries();
      vector<HashNode<ESMFId, ComponentInfo *> *>::iterator it;
      for(it = entries.begin(); it != entries.end(); it++) {
        delete (*it)->getValue();
      }
    }

    if(rc != NULL) *rc = ESMF_SUCCESS;

  }



  ///////////////////// I/O Tracing //////////////////

  //static std::string openFilename;
  //static uint64_t openStartTimestamp = -1;

  void TraceIOOpenStart(const char *path) {
    /*
      if (!traceLocalPet) return;
      openStartTimestamp = TraceGetClock(NULL);
      openFilename = string(path);
    */
  }

  void TraceIOOpenEnd() {
    /*
    if (!traceLocalPet) return;
    uint64_t openEndTimestamp = TraceGetClock(NULL);
    uint64_t openTime = openEndTimestamp - openStartTimestamp;

    esmftrc_default_trace_ioopen(esmftrc_platform_get_default_ctx(),
                                 openFilename.c_str(), openTime);

    openStartTimestamp = -1;
    */
  }

  void TraceIOCloseStart() {
  }

  void TraceIOCloseEnd() {
  }

  void TraceIOWriteStart() {
  }

  void TraceIOWriteEnd(size_t nbytes) {
  }

  void TraceIOReadStart() {
  }

  void TraceIOReadEnd(size_t nbytes) {
  }

  /*
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
  */

  ////////////////////////////////////////////////////

  /////////////////// MPI /////////////////////

  void TraceMPIWaitStart() {
    if (profileLocalPet) {
      currentRegionNode->enteredMPI(TraceGetClock(traceCtx));
    }
  }

  void TraceMPIWaitEnd() {
    if (profileLocalPet) {
      currentRegionNode->exitedMPI(TraceGetClock(traceCtx));
    }
  }

  /*
   * This function used only in tests.
   */
  void TraceTest_GetMPIWaitStats(int *count, long long *time) {
    if (!traceInitialized) return;
    if (profileLocalPet) {
      if (count != NULL)
        *count = currentRegionNode->getCountMPI();
      if (time != NULL)
        *time = currentRegionNode->getTotalMPI();
    }
  }

  void TraceTest_CheckMPIRegion(string name, int *exists) {
    if (exists == NULL) return;
    *exists = 0;
    if (traceLocalPet || profileLocalPet) {
      if (currentRegionNode == NULL) return;
      uint16_t local_id = 0;
      //bool present = userRegionMap.get(name, local_id);

      std::transform(name.begin(), name.end(), name.begin(), ::tolower);
      vector<HashNode<string, uint16_t> *> entries = userRegionMap.getEntries();
      vector<HashNode<string, uint16_t> *>::iterator it;

      bool present = false;
      for(it = entries.begin(); it != entries.end(); it++) {
        string regName = (*it)->getKey();
	std::transform(regName.begin(), regName.end(), regName.begin(), ::tolower);
	//std::cout << "Comparing: " << name << " to " << regName << "\n";
	if (regName == name) {
	  present = true;
	  local_id = (*it)->getValue();
	  break;
	}
      }

      if (!present) return;
      RegionNode *child = currentRegionNode->getChild(local_id);
      if (child != NULL) *exists = 1;
    }
  }


  /////////////////////////////////////////////

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhaseEnter()"
  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase, int *rc) {

    if (traceLocalPet || profileLocalPet) {

      uint16_t local_id = 0;
      ESMFPhaseId phaseId(ESMFId(*ep_vmid, *ep_baseid), *ep_method, *ep_phase);
      bool present = phaseRegionMap.get(phaseId, local_id);
      if (!present) {
        local_id = next_local_id();
        phaseRegionMap.put(phaseId, local_id);
      }

      if (currentRegionNode == NULL) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                      "Trace regions not properly nested", ESMC_CONTEXT, rc);
        return;
      }

      bool added;
      currentRegionNode = currentRegionNode->getOrAddChild(local_id, added);

      //add region to trace output
      if (added && (traceLocalPet || profileOutputToBinary)) {
        esmftrc_default_trace_define_region(esmftrc_platform_get_default_ctx(),
                                            currentRegionNode->getGlobalId(),
                                            TRACE_REGIONTYPE_PHASE,
                                            *ep_vmid, *ep_baseid, *ep_method, *ep_phase,
                                            getRegionNameFromId(local_id).c_str());
      }

      TraceClockLatch(traceCtx);  /* lock in time on clock */
      currentRegionNode->entered(traceCtx->latch_ts);

      if (traceLocalPet) {
        esmftrc_default_trace_regionid_enter(esmftrc_platform_get_default_ctx(),
                                             currentRegionNode->getGlobalId());
      }
      TraceClockUnlatch(traceCtx);

      //printf("OrigPhaseEnter: vmid=%d, bid=%d, method=%d, phase=%d\n", *ep_vmid, *ep_baseid, *ep_method, *ep_phase);

    }

    if (rc != NULL) *rc = ESMF_SUCCESS;

  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::MethodToEnum"
  static inline int MethodToEnum(enum ESMCI::method method) {
      switch(method){
      case ESMCI::METHOD_INITIALIZE:
        return 0;
        break;
      case ESMCI::METHOD_RUN:
        return 1;
        break;
      case ESMCI::METHOD_FINALIZE:
        return 2;
        break;
      default:
        return -1;
        break;
      }
      return -1;
  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventCompPhaseEnter()"
  void TraceEventCompPhaseEnter(Comp *comp, enum method *method, int *phase, int *rc) {

    if (traceLocalPet || profileLocalPet) {
      int localrc;

      int methodid = MethodToEnum(*method);
      if (methodid >= 0) {

        VM *vm;
        localrc = comp->getVm(&vm);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        VMId *vmid = vm->getVMId(&localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        int localvmid = TraceMapVmId(vmid, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        ESMC_Base *base;
        localrc = comp->getBase(&base);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;
        int baseid = base->ESMC_BaseGetID();

        TraceEventPhaseEnter(&localvmid, &baseid, &methodid, phase, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        //printf("CompPhaseEnter: vmid=%d, bid=%d, method=%d, phase=%d\n", localvmid, baseid, methodid, *phase);
      }
    }

    if (rc!=NULL) *rc=ESMF_SUCCESS;

  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventCompPhaseExit()"
  void TraceEventCompPhaseExit(Comp *comp, enum method *method, int *phase, int *rc) {

    if (traceLocalPet || profileLocalPet) {
      int localrc;

      if (*method == ESMCI::METHOD_SETSERVICES || (ESMCI::METHOD_INITIALIZE && *phase==0)) {

        //after SetServices or Init phase 0, look to see if there are any
        //phase map attributes available, and if so record
        //these labels for displaying in the output

        ESMC_Base *base;
        localrc = comp->getBase(&base);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        VM *vm;
        localrc = comp->getVm(&vm);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        VMId *vmid = vm->getVMId(&localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        int localvmid = TraceMapVmId(vmid, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        int baseid = base->ESMC_BaseGetID();
        char *compName = base->ESMC_BaseGetName();

        vector<string> IPM;
        vector<string> IIPM;
        vector<string> RPM;
        vector<string> FPM;

        ESMCI::Info *info = base->ESMC_BaseGetInfo();
        if (info) {
          const std::string nest = "/NUOPC/Instance";
          bool has_nest;
          try {
            has_nest = info->hasKey(nest, true);
          } catch (ESMCI::esmc_error &exc) {
            if (ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;
          }
          if (has_nest) {
            std::map<string, vector<string>*> attrs;
            attrs["InitializePhaseMap"] = &IPM;
            attrs["InternalInitializePhaseMap"] = &IIPM;
            attrs["RunPhaseMap"] = &RPM;
            attrs["FinalizePhaseMap"] = &FPM;
            if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;
              for (std::pair<string, vector<string>*> element : attrs) {
                string key = nest + "/" + element.first;
                bool has_key;
                try {
                  has_key = info->hasKey(key, true);
                } catch (ESMCI::esmc_error &exc) {
                  if (ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;
                }
                if (has_key) {
                  bool is_null = false;
                  try {
                    is_null = info->isNull(key);
                  } catch (ESMCI::esmc_error &exc) {
                    if (ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;
                  }
                  try {
                    if (!is_null) {
                      *(element.second) = info->getvec<std::string>(key, true);
                    }
                  } catch (ESMCI::esmc_error &exc) {
                    if (ESMC_LogDefault.MsgFoundError(exc.getReturnCode(), ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;
                  }
                }
              }
          }
        }
        TraceEventComponentInfo(&localvmid, &baseid, compName, IPM, IIPM, RPM, FPM);
      }

      int methodid = MethodToEnum(*method);
      if (methodid >= 0) {

        VM *vm;
        localrc = comp->getVm(&vm);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        VMId *vmid = vm->getVMId(&localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        int localvmid = TraceMapVmId(vmid, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        ESMC_Base *base;
        localrc = comp->getBase(&base);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;
        int baseid = base->ESMC_BaseGetID();

        TraceEventPhaseExit(&localvmid, &baseid, &methodid, phase, &localrc);
        if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc)) return;

        //printf("CompPhaseExit: vmid=%d, bid=%d, method=%d, phase=%d\n", localvmid, baseid, methodid, *phase);
      }
    }

    if (rc!=NULL) *rc=ESMF_SUCCESS;

  }



#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI:TraceEventPhaseExit()"
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase, int *rc) {

    if (traceLocalPet || profileLocalPet) {

      TraceClockLatch(traceCtx);

      uint16_t local_id = 0;
      ESMFPhaseId phaseId(ESMFId(*ep_vmid, *ep_baseid), *ep_method, *ep_phase);
      bool present = phaseRegionMap.get(phaseId, local_id);  /* should always be present */
      if (!present) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                    "Trace region not properly nested", ESMC_CONTEXT, rc);
        TraceClockUnlatch(traceCtx);
        return;
      }

      if (currentRegionNode == NULL) {
	ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
				      "Trace regions not properly nested", ESMC_CONTEXT, rc);
	TraceClockUnlatch(traceCtx);
	return;
      }
      else if (currentRegionNode->getLocalId() != local_id) {
        stringstream errMsg;
        errMsg << "Trace regions not properly nested exiting from region: ";
        errMsg << getRegionNameFromId(local_id);
        errMsg << " Expected exit from: ";
        errMsg << getRegionNameFromId(currentRegionNode->getLocalId());
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG, errMsg.str().c_str(), ESMC_CONTEXT, rc);
	TraceClockUnlatch(traceCtx);
	return;
      }

      if (traceLocalPet) {
        esmftrc_default_trace_regionid_exit(esmftrc_platform_get_default_ctx(),
                                            currentRegionNode->getGlobalId());
      }

      currentRegionNode->exited(traceCtx->latch_ts);
      currentRegionNode = currentRegionNode->getParent();

      TraceClockUnlatch(traceCtx);
    }

    if (rc!=NULL) *rc = ESMF_SUCCESS;

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

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventRegionEnter()"
  void TraceEventRegionEnter(std::string name, int *rc) {

    if (traceLocalPet || profileLocalPet) {

      uint16_t local_id = 0;
      bool present = userRegionMap.get(name, local_id);
      if (!present) {
        local_id = next_local_id();
        userRegionMap.put(name, local_id);
      }

      if (currentRegionNode == NULL) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
                                      "Trace regions not properly nested", ESMC_CONTEXT, rc);
        return;
      }

      bool added;
      currentRegionNode = currentRegionNode->getOrAddChild(local_id, true, added);

      //add region to trace output
      if (added && (traceLocalPet || profileOutputToBinary)) {
        esmftrc_default_trace_define_region(esmftrc_platform_get_default_ctx(),
                                            currentRegionNode->getGlobalId(),
                                            TRACE_REGIONTYPE_USER,
                                            0, 0, 0, 0,
                                            name.c_str());
      }

      TraceClockLatch(traceCtx);  /* lock in time on clock */
      currentRegionNode->entered(traceCtx->latch_ts);

      if (traceLocalPet) {
        esmftrc_default_trace_regionid_enter(esmftrc_platform_get_default_ctx(),
                                             currentRegionNode->getGlobalId());
      }
      TraceClockUnlatch(traceCtx);

    }

    if (rc != NULL) *rc = ESMF_SUCCESS;

  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventRegionExit()"
  void TraceEventRegionExit(std::string name, int *rc) {

    if (traceLocalPet || profileLocalPet) {
      TraceClockLatch(traceCtx);
      uint16_t local_id = 0;
      bool present = userRegionMap.get(name, local_id);
      if (!present) {
        stringstream errMsg;
        errMsg << "Trace regions not properly nested. Attempt to exit region: ";
        errMsg << name << " that was never entered.";
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG, errMsg.str().c_str(), ESMC_CONTEXT, rc);
        TraceClockUnlatch(traceCtx);
        return;
      }

      if (currentRegionNode == NULL) {
        stringstream errMsg;
        errMsg << "Trace regions not properly nested when attempting to exit region: ";
        errMsg << name << ".";
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG, errMsg.str().c_str(), ESMC_CONTEXT, rc);
	TraceClockUnlatch(traceCtx);
	return;
      }
      else if (currentRegionNode->getLocalId() != local_id) {
        stringstream errMsg;
        errMsg << "Trace regions not properly nested exiting from region: ";
        errMsg << getRegionNameFromId(local_id);
        errMsg << " Expected exit from: ";
        errMsg << getRegionNameFromId(currentRegionNode->getLocalId());
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG, errMsg.str().c_str(), ESMC_CONTEXT, rc);
	TraceClockUnlatch(traceCtx);
	return;
      }

      if (traceLocalPet) {
        esmftrc_default_trace_regionid_exit(esmftrc_platform_get_default_ctx(),
                                            currentRegionNode->getGlobalId());
      }

      currentRegionNode->exited(traceCtx->latch_ts);
      currentRegionNode = currentRegionNode->getParent();

      TraceClockUnlatch(traceCtx);
    }

    if (rc!=NULL) *rc = ESMF_SUCCESS;

  }

  //IPDv00p1=6||IPDv00p2=7||IPDv00p3=4||IPDv00p4=5
  static void UpdateComponentInfoMap(vector<string> phaseMap, ESMFId esmfId, int method, string compName) {
    ComponentInfo *ci = NULL;
    bool present = componentInfoMap.get(esmfId, ci);
    if (!present) {
      ci = new ComponentInfo(esmfId, compName);
      componentInfoMap.put(esmfId, ci);
    }
    if (ci !=NULL) {
      for (unsigned i = 0; i < phaseMap.size(); i++) {
        vector<string> phase = split(trim(phaseMap.at(i)), "=");
        if (phase.size() == 2) {
          int phasenum = -1;
          stringstream ss(trim(phase.at(1)));
          ss >> phasenum;
          if(!(ss.fail())) {
            ci->setPhaseName(ESMFPhaseId(esmfId, method, phasenum), phase.at(0));
            //std::cout << "Added region: " + compName + ", " + phase.at(0) + "\n";
          }
        }
      }
    }
  }


#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::join"
  static string join(vector<string> v) {
    string ret("");
    for (size_t i=0; i<v.size(); i++) {
      if(ret.size() > 0) ret.append("||");
      ret.append(v.at(i));
    }
    return ret;
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventComponentInfo()"
  void TraceEventComponentInfo(int *ep_vmid, int *ep_baseid,
                               const char *ep_name,
                               vector<string> IPM, vector<string> IIPM,
                               vector<string> RPM, vector<string> FPM) {

    if (traceLocalPet || profileOutputToBinary) {
      string strIPM = join(IPM);
      string strRPM = join(RPM);
      string strFPM = join(FPM);
      esmftrc_default_trace_comp(esmftrc_platform_get_default_ctx(),
                                 *ep_vmid, *ep_baseid, ep_name,
                                 strIPM.c_str(), strRPM.c_str(), strFPM.c_str());
    }

    if (profileLocalPet) {
      string compName(ep_name);
      ESMFId esmfId(*ep_vmid, *ep_baseid);
      UpdateComponentInfoMap(IPM, esmfId, 0, compName);
      UpdateComponentInfoMap(IIPM, esmfId, 0, compName);
      UpdateComponentInfoMap(RPM, esmfId, 1, compName);
      UpdateComponentInfoMap(FPM, esmfId, 2, compName);
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
