
#include "conf.h"

#include "ESMC.h"

#include "ESMC_Machine.h"
#include "ESMC_BasicUtil.h"

#ifdef ESMC_HAVE_PTHREADS
#include <pthread.h>
static pthread_t *threadid;
#endif

#ifdef ESMC_HAVE_OMP_THREADS
#include <omp.h>
#endif

#ifdef ESMC_HAVE_MPI
#include <mpi.h>
#endif

static int numthreads;

static int get_thread_num ()
{
  int mythread = 0 ;  /* return value: default zero for non-threaded case */

  int proc, thrd, node;


#ifdef ESMC_HAVE_PTHREADS
  pthread_t mythreadid;  /* thread id from pthreads library */
#endif

/*
** If timers disabled, construct ifdef to avoid function call to things that
** might not be available.
*/


#if ( defined ESMC_HAVE_OMP_THREADS )
  if ((mythread = omp_get_thread_num ()) >= numthreads)
    {
      printf("get_thread_num: returned id %d exceed numthreads %d\n",
	     mythread, numthreads);
      return -1;
    }

#elif ( defined ESMC_HAVE_PTHREADS )

  mythreadid = pthread_self ();

  if (ESMC_BasicUtilLockMutex () < 0)
    {
      printf("get_thread_num: mutex lock failure\n");
      return -1;
    }

  /*
  ** Loop over known physical thread id's.  When my id is found, map it
  ** to logical thread id for indexing.  If not found return a negative
  ** number.
  ** A critical region is necessary because acess to
  ** the array threadid must be by only one thread at a time.
  */

  {
    int n;              /* loop index over number of threads */
    for (n = 0; n < numthreads; n++)
      if (pthread_equal (mythreadid, threadid[n]))
        break;

  /*
  ** If our thread id is not in the known list, add to it after checking that
  ** we do not have too many threads.
  */

    if (n == numthreads) {
      if (numthreads >= ESMC_MACHINE_MAX_THREADS)
	{
	  printf("get_thread_num: numthreads=%d is too big Recompile "
		 "with larger value of MAX_THREADS\n", numthreads);
	  return -1;
	}

      threadid[n] = mythreadid;
      numthreads++;
    }

    if (ESMC_BasicUtilUnlockMutex () < 0)
      {
	printf("get_thread_num: mutex unlock failure\n");
	return -1;
      }

    mythread = n;
  }

#endif


  return mythread;
}


/*************** Interface *****************/


/*--------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_MachineNew"
int ESMC_MachineNew(ESMC_Machine *machine)
{
  int ret;
  
  *machine = (ESMC_Machine) malloc(sizeof(ESMC_MachineClass));

  if ((ret = ESMC_MachineConstruct(*machine)) != ESMC_SUCCESS)
    {
      free(*machine);
      return ret;
    }

  return ESMC_SUCCESS;
}

/*--------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_MachineConstruct"
int ESMC_MachineConstruct(ESMC_Machine machine)
{

  static int initialized = 0;


  if (initialized)
    {
      ESMC_ERRA(ESMC_ERR_BUSY, 0, "There can be only one machine");
    }
  else
    {
      initialized = 1;
    }
  
#ifdef ESMC_HAVE_PTHREADS

  /* Set up threadid table */

  threadid = (pthread_t*) malloc(sizeof(pthread_t) * ESMC_MACHINE_MAX_THREADS);

  threadid[0] = pthread_self();
  numthreads = 1;
#elif defined (ESMC_HAVE_OMP_THREADS)
  numthreads = omp_get_max_threads();
#endif

  return ESMC_SUCCESS;
  
}

/*--------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_MachineDelete"
int ESMC_MachineDelete(ESMC_Machine machine)
{
  int ret;
  
  if ((ret = ESMC_MachineDestruct(machine)) != ESMC_SUCCESS)
    return ret;
  
  free(machine);

  return ESMC_SUCCESS;
}

/*--------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_MachineDestruct"
int ESMC_MachineDestruct(ESMC_Machine machine)
{
#ifdef ESMC_HAVE_PTHREADS
  free(threadid);
#endif

  return ESMC_SUCCESS;
}

/*--------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_MachinePInfo"
int ESMC_MachinePInfo(int *node, int *process, int *thread)
{

  *node = 0;

  *process = 0;

#ifdef ESMC_HAVE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, process);
#endif

  if ((*thread = get_thread_num()) < 0)
  {
     ESMC_ERRA(ESMC_ERR_LIB, 0, "get_thread_num failed");
  }

  return ESMC_SUCCESS;
}
