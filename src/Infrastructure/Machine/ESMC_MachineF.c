/* $Id: ESMC_MachineF.c,v 1.2 2001/11/15 22:56:38 dneckels Exp $ */

#include "ESMC.h"

#include "ESMC_Machine.h"


#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#define FORTRANUNDERSCORE
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_machinenew_ PESMC_MACHINENEW
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_machinenew_ pesmc_machinenew__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_machinenew_ pesmc_machinenew
#else

#define esmc_machinenew_ pesmc_machinenew_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_machinenew_ ESMC_MACHINENEW
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_machinenew_ esmc_machinenew__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_machinenew_ esmc_machinenew
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_machinedelete_ PESMC_MACHINEDELETE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_machinedelete_ pesmc_machinedelete__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_machinedelete_ pesmc_machinedelete
#else

#define esmc_machinedelete_ pesmc_machinedelete_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_machinedelete_ ESMC_MACHINEDELETE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_machinedelete_ esmc_machinedelete__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_machinedelete_ esmc_machinedelete
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_machinepinfo_ PESMC_MACHINEPINFO
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_machinepinfo_ pesmc_machinepinfo__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_machinepinfo_ pesmc_machinepinfo
#else

#define esmc_machinepinfo_ pesmc_machinepinfo_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_machinepinfo_ ESMC_MACHINEPINFO
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_machinepinfo_ esmc_machinepinfo__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_machinepinfo_ esmc_machinepinfo
#endif
#endif

void esmc_machinenew_(ESMC_Machine *machine, int *rc)
{
  *rc = ESMC_MachineNew(machine);
}

void esmc_machinedelete_(ESMC_Machine *machine, int *rc)
{
  *rc = ESMC_MachineDelete(*machine);
}

void esmc_machinepinfo_(int *node, int *process, int *thread, int *rc)
{
  *rc = ESMC_MachinePInfo(node, process, thread);
}
