# Earth System Modeling Framework Dev Container: ubuntu-25.10_gcc-15_mpich

This image provides a ready-to-build Earth System Modeling Framework (ESMF) development environment with GCC 15, CMake, MPICH, NetCDF and Spack.

The Spack externals file is generated during `docker build` from the installed system packages, so the exact `gcc`, `mpich`, `netcdf-c`, and `netcdf-fortran` versions do not need to be hard-coded in the repository.

For general Earth System Modeling Framework project information, see the [ESMF Documentation](https://earthsystemmodeling.org/).

## Installed Toolchain

The Dockerfile currently builds from `ubuntu:25.10` and installs:

- GCC, G++, and GFortran (version 15)
- CMake (installed via apt)
- MPICH (installed via apt)
- NetCDF-C (installed via apt)
- NetCDF-Fortran (installed via apt)
- Spack (version 1.1)

## Environment

The bash shell for `esmfdev` is configured to load the preconfigured ESMF build environment automatically.

| Environment Variable | Description |
|----------------------|-------------|
| `SPACK_ROOT` | Path to the Spack installation |
| `CMAKE_ROOT` | Path to the CMake installation |
| `MPICH_ROOT` | Path to the MPICH installation |
| `NETCDF_C_ROOT` | Path to the NetCDF C installation |
| `NETCDF_FORTRAN_ROOT` | Path to the NetCDF Fortran installation |

## Docker Usage

### Build the image

From `.devcontainer/ubuntu-25.10_gcc-15_mpich`:

```bash
docker build -t esmfdev_ubuntu-25.10_gcc-15_mpich .
```

### Run an interactive shell

```bash
docker run --rm -it --shm-size=2g esmfdev_ubuntu-25.10_gcc-15_mpich bash -l
```

### Run an interactive shell with mounted local esmf folder

From the repository root:

```bash
docker run --rm -it \
	--shm-size=2g \
	-v "$PWD:/home/esmfdev/esmf" \
	-w /home/esmfdev/esmf \
	esmfdev_ubuntu-25.10_gcc-15_mpich \
	bash -l
```

## Build ESMF

With the repository mounted at `/home/esmfdev/esmf`, a typical build is:

```bash
cd /home/esmfdev/esmf
export ESMF_DIR=`pwd`
make -j 4
```

The following options can be added to modify the build using environment variables.
See the [ESMF User's Guide](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_usrdoc/) for more build options.

| Build Option | Description |
|--------------|-------------|
| `ESMF_DIR` | **Required.** Full pathname of the top-level ESMF directory. |
| `ESMF_BOPT` | Build option: `O` for optimized or `g` for debug. |
| `ESMF_COMM` | MPI implementation: `mpich`, `openmpi`, `intelmpi`, `mpiuni`, etc. |
| `ESMF_COMPILER` | Fortran90 and C++ compiler: `gfortran`, `gfortranclang`, `intel`, etc. |
| `ESMF_NETCDF` | NetCDF configuration: `nc-config`, `split`, `standard` |
| `ESMF_PIO` | Parallel I/O library: `internal`, `external`, or `off`. |
| `ESMF_TESTEXHAUSTIVE` | Unit test scope: `ON` builds exhaustive tests; `OFF` builds the basic test set. |

## Test ESMF

After building ESMF, run all ESMF tests:

```bash
make all_tests
```

## Troubleshooting

**Environment variables are missing**

Use a bash shell for the `esmfdev` user, or source `~/.setupenv.sh` manually if you launch a shell that bypasses the normal bash initialization path.

**The container exits immediately**

Run an interactive shell, `-it`.

**Docker is using too much disk space**

Inspect usage with `docker system df` and remove unused images or containers with `docker system prune`.

**System tests `ESMF_ArraySharedDeSSISTest` or `ESMF_FieldSharedDeSSISTest` crash with SIGBUS**

These tests allocate large contiguous shared-memory regions (via `ESMF_PIN_DE_TO_SSI_CONTIG`) backed by `/dev/shm`. Docker's default shared memory size of 64 MB is too small. Run the container with `--shm-size=2g` (or larger) to provide sufficient space.
