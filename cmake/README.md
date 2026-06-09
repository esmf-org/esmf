# CMake Support Provided by ESMF

ESMF installations support user build systems that leverage CMake. Both CMake
modes employed by `find_package(ESMF)` are supported: "config mode"
(recommended!) and "module mode" (supported for backward compatibility).

## CMake Config Mode

In "config mode", CMake tries to locate the `ESMFConfig.cmake` file in standard
locations. During this search, CMake automatically respects paths provided
by the `CMAKE_PREFIX_PATH` variable in the user's environment. Spack-loaded
installations of ESMF automatically set `CMAKE_PREFIX_PATH`, making this the
most convenient method of building CMake-based applications against ESMF.
For ESMF installations not managed by Spack, explicitly add the ESMF
installation root directory to the `CMAKE_PREFIX_PATH` environment variable
for seamless integration with CMake.

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
