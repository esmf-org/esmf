/* $Id: ESMO_Machine.h,v 1.1 2002/11/15 21:25:48 jwolfe Exp $ */

#ifndef ESMC_MACHINE_H
#define ESMC_MACHINE_H

#include "ESMO_BasicUtil.h"

typedef struct
{
  int placeholder;
} ESMC_MachineClass, *ESMC_Machine;

#define ESMC_MACHINE_MAX_THREADS 128

#ifdef __cplusplus
extern "C" {
#endif

int ESMC_MachineNew(ESMC_Machine *machine);
int ESMC_MachineConstruct(ESMC_Machine machine);
int ESMC_MachineDelete(ESMC_Machine machine);
int ESMC_MachineDestruct(ESMC_Machine machine);
int ESMC_MachinePInfo(int *node, int *process, int *thread);

#ifdef __cplusplus
}
#endif

#endif
