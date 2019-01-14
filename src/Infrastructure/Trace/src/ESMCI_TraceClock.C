// $Id$
/*
 * Functions to get the timestamps from the system for trace events
 *
 * Earth System Modeling Framework
 * Copyright 2002-2019, University Corporation for Atmospheric Research, 
 * Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
 * Laboratory, University of Michigan, National Centers for Environmental 
 * Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
 * NASA Goddard Space Flight Center.
 * Licensed under the University of Illinois-NCSA License.
 */

#include <stdint.h>
#include <string>
#include <time.h>

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
#include "ESMCI_VM.h"
#include "ESMCI_Util.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Trace.h"

#define CLOCK_SYNC_TAG 42000
#define RTTMIN_NOTCHANGED_MAX 100
  
namespace ESMCI {

  static int traceClock = 0;
  static int64_t traceClockOffset = 0;

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
  
  
  static int64_t clock_measure_offset(VM *vm, int peerPet, int64_t root_offset) {

    int myPet = vm->getLocalPet();
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

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceClockLatch()"
  void TraceClockLatch(struct esmftrc_platform_filesys_ctx *ctx) {
    switch(traceClock) {
    case ESMF_CLOCK_REALTIME:
      ctx->latch_ts = get_real_clock();
      break;
    case ESMF_CLOCK_MONOTONIC:
      ctx->latch_ts = get_monotonic_raw_clock();
      break;
    case ESMF_CLOCK_MONOTONIC_SYNC:
      ctx->latch_ts = get_monotonic_clock();
      break;
    default:
      ctx->latch_ts = 0;
    }
  }

#undef ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceClockUnlatch()"
  void TraceClockUnlatch(struct esmftrc_platform_filesys_ctx *ctx) {
    ctx->latch_ts = 0;
  }
  
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceGetClock()"
  uint64_t TraceGetClock(void *data) {
    struct esmftrc_platform_filesys_ctx *ctx =
      FROM_VOID_PTR(struct esmftrc_platform_filesys_ctx, data);

    /* if clock is latched, return that value */
    if (ctx->latch_ts != 0) return ctx->latch_ts;
    
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

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::TraceInitializeClock()"  
  void TraceInitializeClock(int *rc) {

    int localrc;
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    
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
    logMsg << "ESMF Trace/Profile clock: " + strClk;
    if (traceClock == ESMF_CLOCK_MONOTONIC_SYNC) {
      logMsg << " (local offset = " << traceClockOffset << ")";
    }
    ESMC_LogDefault.Write(logMsg.str().c_str(), ESMC_LOGMSG_INFO);

  }
  
}

