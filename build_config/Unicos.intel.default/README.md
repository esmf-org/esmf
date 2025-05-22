## Unicos, using the commercial Intel compiler suite, including oneAPI

This covers all Cray systems that utilize the Cray Programming Environment.

Activated by setting: `ESMF_COMPILER=intel`

Defaults to `ESMF_COMM=mpi`. This can be overridden by explicitly setting the
`ESMF_COMM` environment variable. The common setting are:
- mpiuni
- mpi (Cray-MPICH)

### With either `ESMF_COMM` setting
The Cray compiler front-ends are chosen by default:
- `ESMF_F90=ftn`
- `ESMF_CXX=CC`
- `ESMF_C=cc`

These defaults can be overridden by explicitly setting the appropriate
environment variable.

The typical procedure to select between the **classic** and **oneAPI** Intel
compilers is by loading the appropriate compiler module. The details are
system dependent. From an ESMF user perspective it essentially swithes out the
compilers used underneath the Cray compiler wrappers.
