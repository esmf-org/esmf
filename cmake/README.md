# CMake Support Provided by ESMF

ESMF installations support user build systems that leverage CMake. Both CMake
modes employed by `find_package(ESMF)` are supported: "config mode"
(recommended!) and "module mode" (supported for backward compatibility).

## CMake Config Mode

In "config mode", CMake tries to locate the `ESMFConfig.cmake` file in standard
locations. During this search, CMake respects paths provided
by the `CMAKE_PREFIX_PATH` variable in the user's environment. Spack-loaded
installations of ESMF automatically set `CMAKE_PREFIX_PATH`, making this the
most convenient method of building CMake-based applications against ESMF.
For ESMF installations not managed by Spack, explicitly add the ESMF
installation root directory to the `CMAKE_PREFIX_PATH` environment variable
for seamless integration with CMake.

The configuration file provides exported imported targets that transitively
bundle include directories, linked libraries, and compiler definitions (such
as OpenMP or MPI requirements). To consume ESMF targets, utilize the
`find_package()` command in your project's `CMakeLists.txt` file:

```
cmake_minimum_required(VERSION 3.22)
project(MyESMFApplication LANGUAGES Fortran C)

# 1. Locate the ESMF Package
# Specify a minimum version or version constraints if desired
# Explicitly request config mode
find_package(ESMF 9.0.0 REQUIRED CONFIG)

# 2. Define your application target
add_executable(my_model main.F90 physics_mod.F90)

# 3. Link against ESMF Imported Targets
# This automatically manages include paths, RPATHs, and downstream dependencies
target_link_libraries(my_model PUBLIC ESMF::ESMF)
```

Depending on whether the use application code is written in Fortran or C/C++,
the ESMF package defines separate targets to avoid linking mismatch issues.
Target aliases are provided for compatibility.


| Target | Alias | Language Focus | Description |
|----|----|----|----|
| ESMF::ESMF | ESMF::ESMF_Fortran | Fortran | Configured for Fortran libraries and compiler variables, OpenMP flags, and tracking for MPI::MPI_Fortran dependencies. |
| ESMF::ESMC | ESMF::ESMF_C       | C/C++   | Configured with native C/C++ compiler variables, OpenMP flags, and tracking for MPI::MPI_C dependencies. |

## CMake Module Mode

In "module mode", a `FindESMF.cmake` file is needed. The ESMF team provides
CMake[1] modules for convenience. These modules can find libraries, create
targets, and set variables within the scope of a CMake build. They may be used
as is or customized for application-specific build systems. Typically, CMake
module files are copied into a project and maintained along with the project's
build system. For more information about installing the provided modules, see
the ESMF User's Guide[2].

Provided modules:

- FindESMF.cmake

### FindESMF.cmake

The `FindESMF.cmake` module provided by ESMF utilizes a CMake variable,
named ESMFMKFILE, to locate the esmf.mk file. The esmf.mk file is parsed to locate
ESMF libraries and create CMake targets for ESMF. If the ESMF library is not
found then ESMF_FOUND is set to FALSE, a warning is displayed, and find_package
fails (when REQUIRED is set).

If the ESMFMKFILE variable is not set then `FindESMF.cmake` will look for an
environment variable with the same name. If ESMFMKFILE is not set in either
context then CMake will search default paths, including ESMF_ROOT, for the
esmf.mk file.

Usage in CMake:
```
# Where to look for the `FindESMF.cmake` file
#   <PATH_TO_FINDESMF> is to be replaced with the location of `FindESMF.cmake`

list(APPEND CMAKE_MODULE_PATH "<PATH_TO_FINDESMF>")

# How to locate ESMF libraries and create target
#   <X.Y.Z> is to be replaced with the minimum version required

find_package(ESMF <X.Y.Z> MODULE REQUIRED)

# How to link targets
#   <CMAKE_TARGET> is to be replaced with your CMake target

target_link_libraries(<CMAKE_TARGET> PUBLIC ESMF::ESMF)
```

## Links

- [1] https://cmake.org/cmake/help/book/mastering-cmake/index.html
- [2] https://earthsystemmodeling.org/doc/
