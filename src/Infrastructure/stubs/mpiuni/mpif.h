!
!     $Id$
!

!===============================================================================
! This header file is not used in ESMF nor is it intended for user code!
! Please see comment in mpi.c before Fortran symbol section for more details.
! *gjt*
!===============================================================================

!     Trying to provide as little support for fortran code in petsc as needed

!     External objects outside of MPI calls 
       integer, parameter :: MPI_COMM_WORLD = 1
       integer, parameter :: MPI_COMM_SELF = 2
       integer, parameter :: MPI_COMM_NULL = 0
       integer, parameter :: MPI_SUCCESS = 0
       integer, parameter :: MPI_IDENT = 0
       integer, parameter :: MPI_UNEQUAL = 3
       integer, parameter :: MPI_KEYVAL_INVALID = 0
       integer, parameter :: MPI_ERR_OP = 9
       integer, parameter :: MPI_ERR_UNKNOWN = 18
       integer, parameter :: MPI_ERR_INTERN = 21
       integer, parameter :: MPI_STATUS_SIZE=4
       integer, parameter :: MPI_REQUEST_NULL=0

       integer, parameter :: MPI_SOURCE=2,MPI_TAG=3,MPI_ERROR=4

       integer, parameter :: MPI_ANY_SOURCE=4
     
!     Data Types. Same Values used in mpi.c

       integer, parameter :: MPI_INTEGER=0
       integer, parameter :: MPI_REAL=1
       integer, parameter :: MPI_DOUBLE_PRECISION=2
       integer, parameter :: MPI_COMPLEX=3
       integer, parameter :: MPI_CHARACTER=4

       integer, parameter :: MPI_REAL4 = MPI_REAL
       integer, parameter :: MPI_REAL8 = MPI_DOUBLE_PRECISION

       ! The following is non-portable:
       integer, parameter :: MPI_OFFSET_KIND = 8

       integer, parameter :: MPI_MAX_ERROR_STRING = 256

!     Collective operators.  Same values used in mpi.h

       integer, parameter :: MPI_OP_NULL= -1
       integer, parameter :: MPI_SUM    =  0
       integer, parameter :: MPI_MIN    =  1
       integer, parameter :: MPI_MAX    =  2
       integer, parameter :: MPI_PROD   =  3
       integer, parameter :: MPI_LAND   =  4
       integer, parameter :: MPI_BAND   =  5
       integer, parameter :: MPI_LOR    =  6
       integer, parameter :: MPI_BOR    =  7
       integer, parameter :: MPI_LXOR   =  8
       integer, parameter :: MPI_BXOR   =  9

!     Null objects

       integer, parameter :: MPI_DATATYPE_NULL = 0
       integer, parameter :: MPI_INFO_NULL = 0
