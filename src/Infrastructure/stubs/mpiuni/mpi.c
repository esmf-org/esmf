/*$Id$*/

/*
      This provides a few of the MPI-uni functions that cannot be implemented
    with C macros
*/
#include <stddef.h>

#if !defined (ESMF_OS_MinGW)
#include <sys/time.h>
#else
#include <windows.h>
#endif

#include "mpi.h"

#if defined (MPIUNI_USE_STDCALL)
#define MPIUNI_STDCALL __stdcall
#else
#define MPIUNI_STDCALL
#endif

/*
     The system call exit() is not properly prototyped on all systems
   hence we fake it by creating our own prototype
*/

/* This file does not include petsc.h hence cannot use EXTERN_C_XXXX*/
#if defined(__cplusplus)
extern "C" {
  void exit(int);
}
#else
  void exit(int);
#endif

#define MPI_SUCCESS 0
#define MPI_FAILURE 1
void    *MPIUNI_TMP        = 0;
int     MPIUNI_DATASIZE[5] = { sizeof(int),sizeof(float),sizeof(double),2*sizeof(double),sizeof(char)};
/*
       With MPI Uni there is only one communicator, which is called 1.
*/
#define MAX_ATTR 128

typedef struct {
  void                *extra_state;
  void                *attribute_val;
  int                 active;
  MPI_Delete_function *del;
} MPI_Attr;

static MPI_Attr attr[MAX_ATTR];
static int      num_attr = 1,mpi_tag_ub = 100000000;

/* 
   To avoid problems with prototypes to the system memcpy() it is duplicated here

   This version also supports checking for MPI_IN_PLACE
*/
int MPIUNI_Memcpy(void *a,const void* b,int n,enum CheckForMPIInPlace_Flag check_flag) {
  switch(check_flag) {
  case CHECK_FOR_MPI_IN_PLACE_NONE:
    // No pre-check in this case; proceed to the actual memcpy
    break;
  case CHECK_FOR_MPI_IN_PLACE_SOURCE:
    if (b == MPI_IN_PLACE) {
      // If the source is MPI_IN_PLACE, do nothing
      return 0;
    }
    break;
  case CHECK_FOR_MPI_IN_PLACE_DEST:
    if (a == MPI_IN_PLACE) {
      // If the dest is MPI_IN_PLACE, do nothing
      return 0;
    }
    break;
  }

  int  i;
  char *aa= (char*)a;
  char *bb= (char*)b;

  for (i=0; i<n; i++) aa[i] = bb[i];
  return 0;
}

/*
   Used to set the built-in MPI_TAG_UB attribute
*/
static int Keyval_setup(void)
{
  attr[0].active        = 1;
  attr[0].attribute_val = &mpi_tag_ub;
  return 0;
}

/*
         These functions are mapped to the Petsc_ name by ./mpi.h
*/
int Petsc_MPI_Keyval_create(MPI_Copy_function *copy_fn,MPI_Delete_function *delete_fn,int *keyval,void *extra_state)
{
  if (num_attr >= MAX_ATTR) MPI_Abort(MPI_COMM_WORLD,1);

  attr[num_attr].extra_state = extra_state;
  attr[num_attr].del         = delete_fn;
  *keyval                    = num_attr++;
  return 0;
}

int Petsc_MPI_Keyval_free(int *keyval)
{
  attr[*keyval].active = 0;
  return MPI_SUCCESS;
}

int Petsc_MPI_Attr_put(MPI_Comm comm,int keyval,void *attribute_val)
{
  attr[keyval].active        = 1;
  attr[keyval].attribute_val = attribute_val;
  return MPI_SUCCESS;
}
  
int Petsc_MPI_Attr_delete(MPI_Comm comm,int keyval)
{
  if (attr[keyval].active && attr[keyval].del) {
    (*(attr[keyval].del))(comm,keyval,attr[keyval].attribute_val,attr[keyval].extra_state);
  }
  attr[keyval].active        = 0;
  attr[keyval].attribute_val = 0;
  return MPI_SUCCESS;
}

int Petsc_MPI_Attr_get(MPI_Comm comm,int keyval,void *attribute_val,int *flag)
{
  if (!keyval) Keyval_setup();
  *flag                  = attr[keyval].active;
  *(int **)attribute_val = (int *)attr[keyval].attribute_val;
  return MPI_SUCCESS;
}

static int dups = 0;
int Petsc_MPI_Comm_dup(MPI_Comm comm,MPI_Comm *out)
{
  *out = comm;
  dups++;
  return 0;
}

int Petsc_MPI_Comm_free(MPI_Comm *comm)
{
  int i;

  if (--dups) return MPI_SUCCESS;
  for (i=0; i<num_attr; i++) {
    if (attr[i].active && attr[i].del) {
      (*attr[i].del)(*comm,i,attr[i].attribute_val,attr[i].extra_state);
    }
    attr[i].active = 0;
  }
  return MPI_SUCCESS;
}

/* --------------------------------------------------------------------------*/

int Petsc_MPI_Abort(MPI_Comm comm,int errorcode) 
{
  exit(errorcode); 
  return MPI_SUCCESS;
}

static int MPI_was_initialized = 0;

int Petsc_MPI_Initialized(int *flag)
{
  *flag = MPI_was_initialized;
  return 0;
}

static int MPI_was_finalized = 0;

int Petsc_MPI_Finalized(int *flag)
{
  *flag = MPI_was_finalized;
  return 0;
}

/* according to the spec you call MPI_Finalized to
 * see if finalize has already been called, and 
 * MPI_Initialized always returns true once MPI_Init
 * has been called.  the old code here just reset
 * the initialized flag, so i left it in case any
 * apps using this lib depended on that behavior.
 */
int Petsc_MPI_Finalize(void)
{
  MPI_was_initialized = 0;   /* this was how it was */
  MPI_was_finalized = 1;     /* and this line is new */
  return 0;
}

int ESMC_MPI_Alltoallw(void *sendbuf, int *sendcounts, int *sdispls,
                       MPI_Datatype *sendtypes, void *recvbuf, int *recvcounts,
                       int *rdispls, MPI_Datatype *recvtypes, MPI_Comm comm)
{
  // Since we are only implementing this for the single-processor case, the counts, displs
  // and types arguments should all have length 1. We assume that's the case in this
  // implementation.

  // Displacements are not implemented so return an error code if they are non-zero
  if (sdispls[0] != 0 || rdispls[0] != 0) {
    return MPI_ERR_INTERN;
  }

  MPIUNI_Memcpy(recvbuf, sendbuf, sendcounts[0]*sendtypes[0], CHECK_FOR_MPI_IN_PLACE_SOURCE);
  return MPI_SUCCESS;
}

int ESMC_MPI_Scatterv(void *sendbuf, int *sendcounts, int *displs,
                      MPI_Datatype sendtype, void *recvbuf, int recvcount,
                      MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  // Since we are only implementing this for the single-processor case, the sendcounts and
  // displs arguments should have length 1. We assume that's the case in this
  // implementation.

  // Displacements are not implemented so return an error code if they are non-zero
  if (displs[0] != 0) {
    return MPI_ERR_INTERN;
  }

  MPIUNI_Memcpy(recvbuf, sendbuf, sendcounts[0]*sendtype, CHECK_FOR_MPI_IN_PLACE_DEST);
  return MPI_SUCCESS;
}

int ESMC_MPI_Type_create_hvector(int count, int blocklength, MPI_Aint stride,
                                 MPI_Datatype oldtype, MPI_Datatype *newtype)
{
  // Note mpiuni's definition of each datatype as sizeof(raw-type).
  //
  // From some experimentation with a real MPI library, the MPI_Type_size of newtype is
  // independent of the value of stride. Since the MPI_Datatype in mpiuni is just the size
  // of the datatype, we ignore the possible complexity of stride in this implementation.
  *newtype = count*blocklength*oldtype;
  return MPI_SUCCESS;
}

int ESMC_MPI_Type_create_indexed_block(int count, int blocklength,
                                       const int array_of_displacements[],
                                       MPI_Datatype oldtype,
                                       MPI_Datatype *newtype)
{
  // Note mpiuni's definition of each datatype as sizeof(raw-type).
  //
  // From some experimentation with a real MPI library, the MPI_Type_size of newtype is
  // independent of the values in array_of_displacements. Since the MPI_Datatype in mpiuni
  // is just the size of the datatype, we ignore the possible complexity of
  // array_of_displacements in this implementation.
  *newtype = count*blocklength*oldtype;
  return MPI_SUCCESS;
}

int ESMC_MPI_Type_hvector(int count, int blocklength, MPI_Aint stride,
                          MPI_Datatype oldtype, MPI_Datatype *newtype)
{
  // MPI_Type_hvector is a deprecated version of MPI_Type_create_hvector; the only
  // difference is in how stride is specified (bytes vs. elements); since we ignore stride
  // in our implementation of MPI_Type_create_hvector, we can use the same implementation
  // for both.
  return ESMC_MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype);
}

int ESMC_MPI_Type_size(MPI_Datatype datatype, int *size)
{
  // Note that, conveniently, mpiuni defines each datatype as sizeof(raw-type)
  *size = datatype;
  return MPI_SUCCESS;
}

#if !defined (ESMF_OS_MinGW)
// POSIX version
double ESMC_MPI_Wtime(void)
{
  struct timeval tv;
  double seconds;

  gettimeofday(&tv, NULL);
  seconds = tv.tv_sec + tv.tv_usec * 0.000001;
  return seconds;
}
#else
// Windows version
double ESMC_MPI_Wtime(void)
{
  FILETIME ft;
  GetSystemTimeAsFileTime (&ft);

  long long datetime = ft.dwHighDateTime;
  datetime <<= 32;
  datetime += ft.dwLowDateTime;

  return datetime/10000000.;
}
#endif

/*=============================================================================
  I if'ed out the remaining section of this source file in order to prevent
  conflicts with other MPI-stub libraries. PetSc MPIUNI is really not meant
  as a full MPI stub library, but just as a means to an end. For ESMF all that
  matter is to make the ESMF library built in the absense of an MPI 
  implementation. The only portion of ESMF that makes calls into MPI is the
  VM which is written in C++. This makes matter very straight forward because
  the MPIUNI mpi.h will #define the MPI symbols to Petsc_ symbols and there will
  be no symbol conflicts with other MPI stub libraries used for the user code.
  
  ESMF is in the fortunate position of not needing Fortran MPI stub symbols and
  thus I if'ed those out here. 
  
  In conclusion:
  - ESMF uses the Petsc MPIUNI stubs to make the VM compile and link w/o MPI.
  - ESMF symbols in MPIUNI mode do not conflict with other MPI stub libs.
  - ESMF does not provide Fortran nor C MPI stubs for the user
  - There are better/more complete MPI stub libraries available for user code.
  
  *gjt*
==============================================================================*/
#if 0

/* -------------------     Fortran versions of several routines ------------------ */

#if defined(__cplusplus)
extern "C" {
#endif

/******mpi_init*******/
void MPIUNI_STDCALL  mpi_init(int *ierr)
{
  MPI_was_initialized = 1;
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  mpi_init_(int *ierr)
{
  MPI_was_initialized = 1;
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  mpi_init__(int *ierr)
{
  MPI_was_initialized = 1;
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  MPI_INIT(int *ierr)
{
  MPI_was_initialized = 1;
  *ierr = MPI_SUCCESS;
}

/******mpi_finalize*******/
void MPIUNI_STDCALL  mpi_finalize(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  mpi_finalize_(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  mpi_finalize__(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  MPI_FINALIZE(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

/******mpi_barrier*******/
void MPIUNI_STDCALL  mpi_barrier(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  mpi_barrier_(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  mpi_barrier__(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL  MPI_BARRIER(int *ierr)
{
  *ierr = MPI_SUCCESS;
}

/******mpi_comm_size*******/
void MPIUNI_STDCALL mpi_comm_size(MPI_Comm *comm,int *size,int *ierr) 
{
  *size = 1;
  *ierr = 0;
}

void MPIUNI_STDCALL mpi_comm_size_(MPI_Comm *comm,int *size,int *ierr) 
{
  *size = 1;
  *ierr = 0;
}

void MPIUNI_STDCALL mpi_comm_size__(MPI_Comm *comm,int *size,int *ierr) 
{
  *size = 1;
  *ierr = 0;
}

void MPIUNI_STDCALL MPI_COMM_SIZE(MPI_Comm *comm,int *size,int *ierr) 
{
  *size = 1;
  *ierr = 0;
}

/******mpi_comm_rank*******/
void MPIUNI_STDCALL mpi_comm_rank(MPI_Comm *comm,int *rank,int *ierr)
{
  *rank=0;
  *ierr=MPI_SUCCESS;
}

void MPIUNI_STDCALL mpi_comm_rank_(MPI_Comm *comm,int *rank,int *ierr)
{
  *rank=0;
  *ierr=MPI_SUCCESS;
}

void MPIUNI_STDCALL mpi_comm_rank__(MPI_Comm *comm,int *rank,int *ierr)
{
  *rank=0;
  *ierr=MPI_SUCCESS;
}

void MPIUNI_STDCALL MPI_COMM_RANK(MPI_Comm *comm,int *rank,int *ierr)
{
  *rank=0;
  *ierr=MPI_SUCCESS;
}

/******mpi_comm_dup*******/
void MPIUNI_STDCALL mpi_comm_dup(MPI_Comm *comm1,MPI_Comm *comm2,int *ierr)
{
  *ierr=MPI_SUCCESS;
}

void MPIUNI_STDCALL mpi_comm_dup_(MPI_Comm *comm,MPI_Comm *comm2,int *ierr)
{
  *ierr=MPI_SUCCESS;
}

void MPIUNI_STDCALL mpi_comm_dup__(MPI_Comm *comm,MPI_Comm *comm2,int *ierr)
{
  *ierr=MPI_SUCCESS;
}

void MPIUNI_STDCALL MPI_COMM_DUP(MPI_Comm *comm,MPI_Comm *comm2,int *ierr)
{
  *ierr=MPI_SUCCESS;
}

/*******mpi_abort******/
void MPIUNI_STDCALL mpi_abort(MPI_Comm *comm,int *errorcode,int *ierr) 
{
  exit(*errorcode); 
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL mpi_abort_(MPI_Comm *comm,int *errorcode,int *ierr) 
{
  exit(*errorcode);
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL mpi_abort__(MPI_Comm *comm,int *errorcode,int *ierr) 
{
  exit(*errorcode);
  *ierr = MPI_SUCCESS;
}

void MPIUNI_STDCALL MPI_ABORT(MPI_Comm *comm,int *errorcode,int *ierr) 
{
  exit(*errorcode);
  *ierr = MPI_SUCCESS;
}
/*******mpi_allreduce******/
void MPIUNI_STDCALL mpi_allreduce(void *sendbuf,void *recvbuf,int *count,int *datatype,int *op,int *comm,int *ierr) 
{
  if (*op == MPI_OP_NULL) {
    *ierr = MPI_ERR_OP;
    return;
  }
  MPIUNI_Memcpy(recvbuf,sendbuf,(*count)*MPIUNI_DATASIZE[*datatype],CHECK_FOR_MPI_IN_PLACE_SOURCE);
  *ierr = MPI_SUCCESS;
} 
void MPIUNI_STDCALL mpi_allreduce_(void *sendbuf,void *recvbuf,int *count,int *datatype,int *op,int *comm,int *ierr) 
{
  mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr);
} 
void MPIUNI_STDCALL mpi_allreduce__(void *sendbuf,void *recvbuf,int *count,int *datatype,int *op,int *comm,int *ierr) 
{
  mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr);
} 
void MPIUNI_STDCALL MPI_ALLREDUCE(void *sendbuf,void *recvbuf,int *count,int *datatype,int *op,int *comm,int *ierr) 
{
  mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierr);
} 


/* I think the following may be for Fortran linkage, but we should not have
   these symbols in ESMF or else we may conflict with other MPI-stubs!
   gjt */


/* These are some symbols needed by some MPI procs.  These will
   core dump if they try to call these functions, but this at least
   allows compilation, which is the whole point of mpiuni. */
int
	mpi_initialized_,
	mpi_comm_split_,
	mpi_comm_free_,
	mpi_type_indexed_,
	mpi_type_commit_,
	mpi_bcast_,
	mpi_waitall_,
	mpi_recv_,
	mpi_isend_,
	mpi_alltoall_,
	mpi_type_free_,
	mpi_wait_,
	mpi_irecv_;
	



#if defined(__cplusplus)
}
#endif


#endif	


