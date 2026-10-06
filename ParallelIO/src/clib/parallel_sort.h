#ifndef __parallel_sort_h__
#define __parallel_sort_h__

#ifdef __cplusplus
extern "C" {
#endif
#include <mpi.h>
#include <stddef.h>
#include <stdbool.h>

//#define DO_DOUBLE
#ifdef DO_DOUBLE
  typedef double datatype;
#  define MY_MPI_DATATYPE MPI_DOUBLE
#else
  typedef MPI_Offset datatype;
#  define MY_MPI_DATATYPE MPI_OFFSET
#endif

typedef struct {
  datatype *data;
  size_t N;
} CVector;

bool is_unique(CVector v);

    CVector parallel_sort(MPI_Comm comm, CVector v, int *ierr);

int run_unique_check(MPI_Comm comm, size_t N,datatype *v, bool *has_dups);

#ifdef __cplusplus
}
#endif

#endif // #define __parallel_sort_h__
