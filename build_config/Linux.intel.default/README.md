## Linux, using the commercial Intel compiler suite, including oneAPI

Activated by setting: `ESMF_COMPILER=intel`

Defaults to `ESMF_COMM=mpiuni`. This can be overridden by explicitly setting the
`ESMF_COMM` environment variable.
The common setting are:
- mpiuni
- intelmpi
- mpich
- openmpi
- mpt

### With `ESMF_COMM=mpiuni`
The **classic** Intel compiler front-ends are chosen by default:
- `ESMF_F90=ifort`
- `ESMF_CXX=icpc`
- `ESMF_C=icc`

These defaults can be overridden by explicitly setting the appropriate
environment variable.

To choose a *combination* of **classic** Fortran and **oneAPI** C/C++ set:
- `ESMF_CXX=icpx`
- `ESMF_C=icx`

To choose **oneAPI** Fortran and C/C++ front-ends set:
- `ESMF_F90=ifx`
- `ESMF_CXX=icpx`
- `ESMF_C=icx``

### With `ESMF_COMM=intelmpi`
The IntelMPI compiler front-ends are chosen by default:
- `ESMF_F90=mpiifort`
- `ESMF_CXX=mpiicpc`
- `ESMF_C=mpiicc`

These defaults can be overridden by explicitly setting the appropriate
environment variable.

Notice that it depends on the specific IntelMPI installation which actual
compilers, classic vs. oneAPI, are chosen by default under each MPI wrapper.
The IntelMPI defaults can be overridden using the `I_MPI_F90`, `I_MPI_CXX`,
and `I_MPI_CC` environment variables.

To choose **classic** Fortran and C/C++ set:
- `I_MPI_F90=<path>/ifort`
- `I_MPI_CXX=<path>/icpc`
- `I_MPI_CC=<path>/icc`

To choose **oneAPI** Fortran and C/C++ set:
- `I_MPI_F90=<path>/ifx`
- `I_MPI_CXX=<path>/icpx`
- `I_MPI_CC=<path>/icx`

### With `ESMF_COMM=mpich`
The MPICH compiler front-ends are chosen by default:
- `ESMF_F90=mpif90`
- `ESMF_CXX=mpicxx`
- `ESMF_C=mpicc`

These defaults can be overridden by explicitly setting the appropriate
environment variable.

### With `ESMF_COMM=openmpi`
The OpenMPI compiler front-ends are chosen by default:
- `ESMF_F90=mpifort` if present, or `ESMF_F90=mpif90` otherwise
- `ESMF_CXX=mpicxx`
- `ESMF_C=mpicc`

These defaults can be overridden by explicitly setting the appropriate
environment variable.

### With `ESMF_COMM=mpt`
The MPT compiler front-ends are chosen by default:
- `ESMF_F90=mpif90`
- `ESMF_CXX=mpicxx`
- `ESMF_C=mpicc`

These defaults can be overridden by explicitly setting the appropriate
environment variable.
