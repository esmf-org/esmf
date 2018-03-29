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

#include "ESMCI_Macros.h"
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_Trace.h"
#include "ESMCI_Comp.h"
#include "ESMCI_VMKernel.h"
#include "ESMCI_HashMap.h"

#define BT_CHK(_value, _ctx)                                            \
  if ((_value) != 0) {                                                  \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, "Internal tracing error", _ctx, rc); \
    return;}

#define BT_CHK_NULL(_value, _ctx)                                       \
  if ((_value) == NULL) {                                               \
    ESMC_LogDefault.MsgFoundError(ESMC_RC_LIB, "Internal tracing error", _ctx, rc); \
    return;}

#ifndef ESMF_OS_MinGW
#define TRACE_DIR_PERMISSIONS (S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH)
#else
#define TRACE_DIR_PERMISSIONS (S_IRWXU)
#endif

#define NODENAME_LEN 100

#ifdef ESMF_BABELTRACE
#include <babeltrace/ctf-writer/writer.h>
#include <babeltrace/ctf-writer/clock.h>
#include <babeltrace/ctf-writer/stream.h>
#include <babeltrace/ctf-writer/event.h>
#include <babeltrace/ctf-writer/event-types.h>
#include <babeltrace/ctf-writer/event-fields.h>

#define ESMF_BT_COMMON_UUID
#ifdef ESMF_BT_COMMON_UUID
/* Note: these explicit definitions are included
 * somewhat temporarily so that we can write an
 * explict uuid to the trace.  This is necessary
 * so that all traces in a multi-PET system have
 * the same uuid and their event streams can
 * therefore be easily shared.
 *
 * A later version of Babeltrace will support
 * explicitly setting this.
 */

typedef unsigned char uuid_t[16];

struct bt_ref {
  long count;
  void *release;
};

struct bt_object {
  struct bt_ref ref_count;
  void *release;
  void *parent;
};

struct bt_ctf_trace {
  struct bt_object base;
  int frozen;
    unsigned char uuid[16];
};

struct bt_ctf_writer {
  struct bt_object base;
  int frozen;
  struct bt_ctf_trace *trace;
};
#endif /* ESMF_BT_COMMON_UUID */

#define ESMF_BT_SINGLE_TRACE
#ifdef ESMF_BT_SINGLE_TRACE
struct bt_ctf_stream_class {
  struct bt_object base;
  void * /*GString **/ name;
  struct bt_ctf_clock *clock;
  void * /*GPtrArray **/ event_classes;
  int id_set;
  uint32_t id;
  uint32_t next_event_id;
  uint32_t next_stream_id;
  struct bt_ctf_field_type *packet_context_type;
  struct bt_ctf_field_type *event_header_type;
  struct bt_ctf_field_type *event_context_type;
  int frozen;
  int byte_order;
  int valid;
};
#endif /* ESMF_BT_SINGLE_TRACE */
#else /* ESMF_BABELTRACE */

#define ESMF_TRACE_INTERNAL /* enable internal tracing */
#ifdef ESMF_TRACE_INTERNAL

#include <esmftrc.h>
#define EVENT_BUF_SIZE_DEFAULT 4096
#define EVENT_BUF_SIZE_EAGER 1024

#ifdef __cplusplus
# define TO_VOID_PTR(_value)            static_cast<void *>(_value)
# define FROM_VOID_PTR(_type, _value)   static_cast<_type *>(_value)
#else
# define TO_VOID_PTR(_value)            ((void *) (_value))
# define FROM_VOID_PTR(_type, _value)   ((_type *) (_value))
#endif
#endif /* ESMF_TRACE_INTERNAL */

#endif /* ESMF_BABELTRACE */

using std::string;
using std::vector;
using std::stringstream;

#define REGION_HASHTABLE_SIZE 100

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

  static bool traceLocalPet = false;
  static int traceClock = 0;
  static int64_t traceClockOffset = 0;
  static HashMap<string, int, REGION_HASHTABLE_SIZE, StringHashF> regionMap;
  static int nextRegionId = 1;

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


#ifdef ESMF_BABELTRACE
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
  struct bt_ctf_field_type *bt_type_enum_region;

  //event classes
  struct bt_ctf_event_class *bt_event_class_control;
  struct bt_ctf_event_class *bt_event_class_comp;
  struct bt_ctf_event_class *bt_event_class_region;

  static int flushStream = 0;

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::FlushStream()"
  static void FlushStream() {
    int localrc;
    int *rc = &localrc;
    flushStream++;
    if (flushStream >= BT_FLUSH_STREAM_INTERVAL) {
      BT_CHK(bt_ctf_stream_flush(bt_stream), ESMC_CONTEXT);
      flushStream = 0;
    }
  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceOpen()"
  void TraceOpen(const char *trace_dir, int *rc) {

    int localrc;
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

    VM *globalvm = VM::getGlobal(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
          ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
      return;
    int stream_id = globalvm->getMypet();


    struct stat st = {0};
    if (stream_id == 0) {
      if (stat(stream_dir_root, &st) == -1) {
        if (mkdir(stream_dir_root, TRACE_DIR_PERMISSIONS) == -1) {      
          //perror("mkdir()");
          ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error creating trace root directory",
                                        ESMC_CONTEXT, rc);
          return;
        }
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

#ifndef ESMF_BT_SINGLE_TRACE
    //my specific directory
    sprintf(stream_dir, "%s/PET%03d", stream_dir_root, stream_id);

    if (stat(stream_dir, &st) == -1) {
      if (mkdir(stream_dir, TRACE_DIR_PERMISSIONS) == -1) {
        //perror("mkdir()");
        ESMC_LogDefault.MsgFoundError(ESMC_RC_FILE_CREATE, "Error creating trace PET directory",
                                      ESMC_CONTEXT, rc);
        return;
      }
    }

    bt_writer = bt_ctf_writer_create(stream_dir);
    BT_CHK_NULL(bt_writer, ESMC_CONTEXT);
#else
    bt_writer = bt_ctf_writer_create(stream_dir_root);
    BT_CHK_NULL(bt_writer, ESMC_CONTEXT);
#endif

#ifdef ESMF_BT_COMMON_UUID
    //set common UUID
    const unsigned char my_custom_uuid[16] = {
      0xb3, 0x38, 0x4d, 0x7b, 0x77, 0x8a, 0x4f, 0xf0,
      0x99, 0xbd, 0x43, 0x54, 0x9e, 0xc0, 0x54, 0x1b,
    };
    memcpy(bt_writer->trace->uuid, my_custom_uuid, 16);
    // end UUID setting
#endif

    bt_clock = bt_ctf_clock_create("sys_clock");
    BT_CHK_NULL(bt_clock, ESMC_CONTEXT);

    BT_CHK(bt_ctf_writer_add_clock(bt_writer, bt_clock), ESMC_CONTEXT);

    TraceSetupTypes(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc,
                                      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, rc))
      return;

#ifdef ESMF_BT_SINGLE_TRACE
    bt_stream_class->next_stream_id = stream_id;
#endif

    bt_stream = bt_ctf_writer_create_stream(bt_writer, bt_stream_class);
    BT_CHK_NULL(bt_stream, ESMC_CONTEXT);

    struct bt_ctf_field *packet_context = bt_ctf_stream_get_packet_context(bt_stream);
    BT_CHK_NULL(packet_context, ESMC_CONTEXT);

    struct bt_ctf_field *field_pet = bt_ctf_field_structure_get_field(packet_context, "pet");
    BT_CHK_NULL(field_pet, ESMC_CONTEXT);

    BT_CHK(bt_ctf_field_unsigned_integer_set_value(field_pet, stream_id), ESMC_CONTEXT);

    BT_CHK(bt_ctf_writer_add_environment_field(bt_writer, "esmf_trace_version", BT_ESMF_TRACE_VERSION), ESMC_CONTEXT);

    //write out metadata file
#ifdef ESMF_BT_SINGLE_TRACE
    if (stream_id == 0) {
      bt_ctf_writer_flush_metadata(bt_writer);
    }
#else
    bt_ctf_writer_flush_metadata(bt_writer);
#endif

    //reference counting
    bt_ctf_field_put(packet_context);
    bt_ctf_field_put(field_pet);

  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceClose()"
  void TraceClose(int *rc) {

    if (rc != NULL) *rc = ESMF_SUCCESS;

    if (!traceLocalPet) return;

    BT_CHK(bt_ctf_stream_flush(bt_stream), ESMC_CONTEXT);

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
    bt_ctf_field_type_put(bt_type_enum_region);

    bt_ctf_event_class_put(bt_event_class_control);
    bt_ctf_event_class_put(bt_event_class_comp);
    bt_ctf_event_class_put(bt_event_class_region);

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

    //region event class
    bt_event_class_region = bt_ctf_event_class_create("region");
    BT_CHK_NULL(bt_event_class_region, ESMC_CONTEXT);

    bt_type_enum_region = bt_ctf_field_type_enumeration_create(bt_type_uint4);
    BT_CHK_NULL(bt_type_enum_region, ESMC_CONTEXT);

    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_region, "enter", BT_REGION_ENTER, BT_REGION_ENTER);
    bt_ctf_field_type_enumeration_add_mapping(bt_type_enum_region, "exit", BT_REGION_EXIT, BT_REGION_EXIT);

    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_region, bt_type_enum_region, "ctrl"), ESMC_CONTEXT);
    BT_CHK(bt_ctf_event_class_add_field(bt_event_class_region, bt_type_string, "name"), ESMC_CONTEXT);

    BT_CHK(bt_ctf_stream_class_add_event_class(bt_stream_class, bt_event_class_region), ESMC_CONTEXT);


    //component event class
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


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhase()"
  void TraceEventPhase
     (
      int ctrl,
      int *ep_vmid,
      int *ep_baseid,
      int *ep_method,
      int *ep_phase
      )
  {

    if (!traceLocalPet) return;
                        
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

    BT_CHK(bt_ctf_clock_set_time(bt_clock, get_clock(NULL)), ESMC_CONTEXT);

    BT_CHK(bt_ctf_stream_append_event(bt_stream, event), ESMC_CONTEXT);
    FlushStream();

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

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventComponentInfo()"
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

    if (!traceLocalPet) return;

    int localrc;
    int *rc = &localrc;

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

    BT_CHK(bt_ctf_clock_set_time(bt_clock, get_clock(NULL)), ESMC_CONTEXT);

    BT_CHK(bt_ctf_stream_append_event(bt_stream, event), ESMC_CONTEXT);

    FlushStream();

    bt_ctf_event_put(event);
    bt_ctf_field_put(field_name);
    bt_ctf_field_put(field_vmid);
    bt_ctf_field_put(field_baseid);
    bt_ctf_field_put(field_attr_len);

  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventRegion()"
  void TraceEventRegion(int ctrl, const char *name) {

    if (!traceLocalPet) return;

    int localrc;
    int *rc = &localrc;

    struct bt_ctf_event *event = bt_ctf_event_create(bt_event_class_region);
    BT_CHK_NULL(event, ESMC_CONTEXT);

    struct bt_ctf_field *field_ctrl = bt_ctf_event_get_payload(event, "ctrl");
    BT_CHK_NULL(field_ctrl, ESMC_CONTEXT);
    struct bt_ctf_field *ctrl_container_field = bt_ctf_field_enumeration_get_container(field_ctrl);
    BT_CHK_NULL(ctrl_container_field, ESMC_CONTEXT);
    BT_CHK(bt_ctf_field_unsigned_integer_set_value(ctrl_container_field, ctrl), ESMC_CONTEXT);

    struct bt_ctf_field *field_name = bt_ctf_event_get_payload(event, "name");
    BT_CHK(bt_ctf_field_string_set_value(field_name, name), ESMC_CONTEXT);

    BT_CHK(bt_ctf_clock_set_time(bt_clock, get_clock(NULL)), ESMC_CONTEXT);

    BT_CHK(bt_ctf_stream_append_event(bt_stream, event), ESMC_CONTEXT);

    FlushStream();

    bt_ctf_event_put(event);
    bt_ctf_field_put(field_ctrl);
    bt_ctf_field_put(ctrl_container_field);
    bt_ctf_field_put(field_name);

  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventMemInfo()"
  void TraceEventMemInfo() {
    /* ignore for now */
  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventClock()"
  void TraceEventClock(int *ep_year, int *ep_month, int *ep_day,
                       int *ep_hour, int *ep_minute, int *ep_second)
  { /* ignore for now */ }


#else  /* ESMF_BABELTRACE not set */

#ifdef ESMF_TRACE_INTERNAL

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

  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceClose()"
  void TraceClose(int *rc) {
    struct esmftrc_platform_filesys_ctx *ctx = g_esmftrc_platform_filesys_ctx;

    if(rc != NULL) *rc = ESMF_SUCCESS;

    // allow multiple calls to TraceClose for system test
    // ignore any call after the first one

    if (ctx != NULL) {

      if (esmftrc_packet_is_open(&ctx->ctx) &&
          !esmftrc_packet_is_empty(&ctx->ctx)) {
        close_packet(ctx);
      }

      fclose(ctx->fh);
      free(esmftrc_packet_buf(&ctx->ctx));
      free(ctx);
      g_esmftrc_platform_filesys_ctx = NULL;

      for (int i=0; i < nextVmId; i++) {
        (&vmIdMap[i])->destroy();
      }

    }

  }

  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;
    esmftrc_default_trace_phase_enter(esmftrc_platform_get_default_ctx(),
                                      *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;
    esmftrc_default_trace_phase_exit(esmftrc_platform_get_default_ctx(),
                                     *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    if (!traceLocalPet) return;
    esmftrc_default_trace_prologue_enter(esmftrc_platform_get_default_ctx(),
                                         *ep_vmid, *ep_baseid, *ep_method, *ep_phase);
  }

  //void TraceEventPhasePrologueExit(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    //TraceEventPhase(BT_CNTL_ENDP, ep_vmid, ep_baseid, ep_method, ep_phase);
  //}

  //void TraceEventPhaseEpilogueEnter(int *ep_vmid, int *ep_baseid, int *ep_method, int *ep_phase) {
    //TraceEventPhase(BT_CNTL_STARTE, ep_vmid, ep_baseid, ep_method, ep_phase);
  //}

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
      //printf("added new region: %s = %d\n", name.c_str(), region_id);
      //add definition to trace
      esmftrc_default_trace_define_region(esmftrc_platform_get_default_ctx(),
                                          region_id,
                                          name.c_str());
    }

    esmftrc_default_trace_regionid_enter(esmftrc_platform_get_default_ctx(),
                                         region_id);

    //esmftrc_default_trace_region_enter(esmftrc_platform_get_default_ctx(),
    //                                   name.c_str());
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


    esmftrc_default_trace_regionid_exit(esmftrc_platform_get_default_ctx(),
                                         region_id);

    //esmftrc_default_trace_region_exit(esmftrc_platform_get_default_ctx(),
    //                                   name);
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


////////////////////////////////////////////////////////////////////////////////

#else /* no ESMF_BABELTRACE or ESMF_TRACE_INTERNAL - just use stubs */

#define LOG_NO_BT_LIB ESMC_LogDefault.MsgFoundError(ESMF_RC_LIB_NOT_PRESENT, \
                "The Babeltrace library is required for tracing but is not present.",\
                ESMC_CONTEXT, rc);

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceOpen()"
  void TraceOpen(const char *trace_dir, int *rc) { LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceClose()"
  void TraceClose(int *rc) { LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceSetupTypes()"
  void TraceSetupTypes(int *rc) { LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhase()"
  void TraceEventPhase(int ctrl, int *ep_vmid,
                       int *ep_baseid, int *ep_method, int *ep_phase)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhaseEnter()"
  void TraceEventPhaseEnter(int *ep_vmid, int *ep_baseid,
                            int *ep_method, int *ep_phase)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhaseExit()"
  void TraceEventPhaseExit(int *ep_vmid, int *ep_baseid,
                           int *ep_method, int *ep_phase)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhasePrologueEnter()"
  void TraceEventPhasePrologueEnter(int *ep_vmid, int *ep_baseid,
                            int *ep_method, int *ep_phase)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhasePrologueExit()"
  void TraceEventPhasePrologueExit(int *ep_vmid, int *ep_baseid,
                                   int *ep_method, int *ep_phase)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventEpilogueEnter()"
  void TraceEventPhaseEpilogueEnter(int *ep_vmid, int *ep_baseid,
                                    int *ep_method, int *ep_phase)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventPhaseEpilogueExit()"
  void TraceEventPhaseEpilogueExit(int *ep_vmid, int *ep_baseid,
                                   int *ep_method, int *ep_phase)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventComponentInfo()"
  void TraceEventComponentInfo(Comp *comp, int *ep_vmid, int *ep_baseid,
                               const char *ep_name, std::string attributeKeys, std::string attributeVals)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventRegion()"
  void TraceEventRegion(int ctrl, const char*name)
  { int *rc=NULL; LOG_NO_BT_LIB }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventMemInfo()"
  void TraceEventMemInfo()

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceEventClock()"
    void TraceEventClock(int *ep_year, int *ep_month, int *ep_day,
                         int *ep_hour, int *ep_minute, int *ep_second)
  { int *rc=NULL; LOG_NO_BT_LIB }

#endif /* ESMF_TRACE_INTERNAL */
#endif /* ESMF_BABELTRACE */

}

