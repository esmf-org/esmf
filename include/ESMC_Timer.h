/* $Id: ESMC_Timer.h,v 1.3 2001/12/07 20:33:42 dneckels Exp $ */

#ifndef ESMC_PROFILER_H
#define ESMC_PROFILER_H

#include <stdarg.h>

#include "ESMC_BasicUtil.h"

#define ESMC_ALL_TIMERS "*timers*"

#define ESMC_PROFILER_MAX_CHARS 15


typedef enum {
  ESMC_RESERVED = 0,
  ESMC_USRSYS = 1,
  ESMC_WALL = 2,
  ESMC_RESERVED1 = 3,
  ESMC_LD1CACHE_MISS = 4,
  ESMC_LD2CACHE_MISS = 5,
  ESMC_CYCLES = 6,
  ESMC_ELAPSED_CYCLES = 7,
  ESMC_FP_INSTR = 8,
  ESMC_LOADSTORE_INSTR = 9,
  ESMC_INSTR = 10,
  ESMC_STALL = 11,
  ESMC_RESERVED2 = 12,
  ESMC_STUB_TIMERS = 13
} ESMC_TimerOption;



#include "ESMC_Log.h"

#ifdef __cplusplus
extern "C" {
#endif
  
int ESMC_TimerInit(char *name, ESMC_TimerOption option1, ...);
int ESMC_TimerStart(char *name);
int ESMC_TimerStop(char *name);
int ESMC_TimerPrint(char *name, ESMC_Log log);
int ESMC_TimerSetSTDLog(ESMC_Log log);

#ifdef __cplusplus
}
#endif

#endif
