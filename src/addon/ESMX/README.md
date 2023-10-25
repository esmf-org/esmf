# ESMX

ESMX is the **E**arth **S**ystem **M**odel e**X**ecutable layer.

http://earthsystemmodeling.org/esmx/

The ESMX layer is built on top of the ESMF and NUOPC APIs.

The idea behind ESMX is to make it as simple as possible for a user to build, run, and test NUOPC based systems. The approach implemented is the same whether applied to a single component, or a fully coupled system of NUOPC-compliant components. ESMX user interfaces are implemented through [YAML](https://yaml.org/) based configuration files.

Major objectives of ESMX are:
 - **Simplification** of standing up new NUOPC-based systems.
 - **Promotion** of hierarchical model component testing.
 - **Reduction** of maintenance cost for established NUOPC-based systems.
 - **Improved** alignment and interoperability between different NUOPC-based systems. (Configuration files, build procedures, etc.)
 - **Faster and more coordinated** roll-out and adoption of new NUOPC/ESMF features.

ESMX unifies, provides, and maintains those parts of NUOPC-based modeling systems that are common across most such implementations. This includes the top level application and driver code, parts of the build infrastructure, and tools to manage run configurations.

## The Unified Driver Executable

One of the main pieces provided by ESMX is the *unified driver executable*. A good starting point to explore this feature is the [ESMX_AtmOcnProto](https://github.com/esmf-org/nuopc-app-prototypes/tree/develop/ESMX_AtmOcnProto) example under the NUOPC prototype repository.

The unified driver executable created by ESMX is defined by `exe_name` under `Application Options` (default: esmx_app). `ESMX_EXE_NAME` will be used for the remainder of this section to refer to the unfied driver executable.

### ESMX_Builder

The ESMX_Builder is a shell script included with ESMF installations that facilitates building ESMX. The script is installed into the ESMF binary directory within an ESMF installation. For more information on installing ESMF see the [ESMF User's Guide](https://earthsystemmodeling.org/doc/).

If the ESMF binary directory is included in the PATH environment variable then ESMX_Builder can be called from any directory as follows:

```
ESMX_Builder [OPTIONS ...] [ESMX_BUILD_FILE]

options:
  [--esmx-dir=ESMF_ESMXDIR]
  [--esmfmkfile=ESMFMKFILE]
  [--build-dir=BUILD_DIR]
  [--prefix=INSTALL_PREFIX]
  [--disable-comps=DISABLE_COMPS]
  [--build-type=BUILD_TYPE] or [-g]
  [--build-args=BUILD_ARGS]
  [--build-jobs=JOBS]
  [--load-modulefile=MODULEFILE]
  [--load-bashenv=BASHENV]
  [--test[=TEST_ARGS]] or [-t[=TEST_ARGS]]
  [--verbose] or [-v]

where:
  ESMX_BUILD_FILE          ESMX build configuration file
                           (default: esmxBuild.yaml)

  --esmx-dir=ESMF_ESMXDIR  ESMX source directory
                           (default: use ESMF_ESMXDIR in ESMFMKFILE)

  --esmfmkfile=ESMFMKFILE  ESMF makefile fragment
                           (default: use environment)

  --build-dir=BUILD_DIR    build directory
                           (default: build)

  --prefix=INSTALL_PREFIX  installation prefix
                           (default: install)

  --disable-comps=DISABLE_COMPS  disable components (e.g. ESMX_Data)

  --build-type=BUILD_TYPE  build type; valid options are 'debug', 'release', 'relWithDebInfo'
  -g                       (default: release) (-g sets BUILD_TYPE to debug)

  --build-args=BUILD_ARGS  global cmake arguments (e.g. -DVAR=val)

  --build-jobs=BUILD_JOBS  number of jobs used for building esmx and components

  --load-module=MODULEFILE load modulefile before building

  --load-bashenv=BASHENV   load bash environment file before building

  --test[=TEST_ARGS] or    (beta) run ctest suite for ESMX Driver, pass TEST_ARGS to ctest
  -t[=TEST_ARGS]

  --verbose or -v          build with verbose output
```

This script installs `ESMX_EXE_NAME` into INSTALL_PREFIX/bin.

### ESMX Build Configuration

The ESMX build system depends on a build file defined by the ESMX_BUILD_FILE variable. When unspecified the ESMX_BUILD_FILE defaults `esmxBuild.yaml`. This is a [YAML](https://yaml.org/) file with a very simple format. An example is given here:

```
application:

  disable_comps: ESMX_Data
  cmake_build_args: -DOPTION=VALUE

components:

  TaWaS:
    cmake_config: TaWaS/tawas.cmake
    fort_module:  tawas.mod

  Lumo:
    cmake_config: Lumo/lumo.cmake

tests:

  TestTawasLumo:
    dir: path/to/test/data
```

In this example two components are built into `ESMX_EXE_NAME` explicitly. (Read about [dynamically loading of components from shared objects at run-time](#dynamically-loading-components-from-shared-objects-at-run-time) later.) Each component is given a name, here `TaWaS` and `Lumo`, respectively. Components will be referenced by this *component-name* in the run-time configuration (`esmxRun.yaml`) discussed below.

**CAUTION:** Component names are case-sensitive when used e.g. by default to construct library names, etc. However, they are treated case-insensitive when referenced from within the `esmxRun.yaml` file due to the case-insensitive nature of Fortran when referencing modules via the USE statement.

ESMX comes with a default data component called `ESMX_Data`. It is built into `ESMX_EXE_NAME` by default. This example disables this behavior by setting `disable_comps: ESMX_Data`.

There are *three* top level sections recognized in the ESMX build file. Each is introduced by a specific key using the [YAML](https://yaml.org/) map syntax: `application:`, `components:`, and `tests:`. The value associated with each key is a map of key/value pairs. The available option keys under each top level section are discussed below.

#### Application Options (`application` key)

These options affect the ESMX application layer. If no key/value pair is provided then the default will be used.

| Option key            | Description / Value options                           | Default                |
| --------------------- | ----------------------------------------------------- | ---------------------- |
| `exe_name`            | executable name for application                       | `esmx_app`             |
| `disable_comps`       | scalar or list of components to disable               | *None*                 |
| `link_module_paths`   | scalar or list of search paths for CMake modules      | *None*                 |
| `link_packages`       | scalar or list of cmake packages, linked to esmx      | *None*                 |
| `link_paths`          | scalar or list of search path for external libraries  | *None*                 |
| `link_libraries`      | scalar or list of external libraries, linked to esmx  | *None*                 |
| `build_args`          | scalar or list of arguments passed to all build_types | *None*                 |
| `build_jobs`          | job number used for all build_types                   | *None*                 |
| `build_verbose`       | verbosity setting used for all build_types            | *None*                 |
| `cmake_build_args`    | scalar or list of argumens passed to all cmake builds | *None*                 |
| `cmake_build_jobs`    | job number used for all cmake builds                  | *None*                 |
| `cmake_build_verbose` | verbosity setting used for all cmake builds           | *None*                 |
| `make_build_args`     | scalar or list of argumens passed to all make builds  | *None*                 |
| `make_build_jobs`     | job number used for all make builds                   | *None*                 |
| `script_build_args`   | scalar or list of argumens passed to all script builds| *None*                 |
| `test`                | (beta) add test cases: `on` or `off`                  | `off`                  |
| `test_exe`            | (beta) executable used to launch test cases           | ESMF_INTERNAL_MPIRUN   |
| `test_dir`            | (beta) output directory for test cases                | CMAKE_BINARY_DIR-tests |
| `test_tasks`          | (beta) number of tasks used to run test cases         | `4`                    |

#### Component Options (`components` key)

This section contains a key for for each *component-name*, specifying component specific options. If no key/value pair is provided then the default will be used.

| Option key       | Description / Value options                   | Default                |
| ---------------- | --------------------------------------------- | ---------------------- |
| `build_type`     | [build type:](#build-types) `auto`, `cmake`, `make`, `script`, `none`     | `auto`                 |
| `build_script`   | build script                                  | `compile`              |
| `build_args`     | scalar or list of arguments for building component                        | *None*                 |
| `source_dir`     | source directory for build                    | *component-name*       |
| `cmake_config`   | CMake configuration file                      | *component-name*.cmake |
| `libraries`      | component libraries, linked to esmx           | *component-name*       |
| `fort_module`    | fortran module filename for NUOPC SetServices | *component-name*.mod   |
| `install_prefix` | root directory for installation               | `install`              |
| `config_dir`     | subdirectory for cmake configuration file     | `cmake`                |
| `library_dir`    | subdirectory for library file                 | `lib`                  |
| `include_dir`    | subdirectory for fortran module file          | `include`              |
| `link_paths`     | search path for external libraries            | *None*                 |
| `link_libraries` | external libraries, linked to esmx            | *None*                 |
| `git_repository` | URL for downloading git repository            | *None*                 |
| `git_tag`        | tag for downloading git repository            | *None*                 |
| `git_dir`        | download directory for git repository         | *None*                 |
| `test_dir`       | (beta) directory used for test case data      | *None*                 |
| `test_exe`       | (beta) executable used to run test case       | ESMX_TEST_EXE          |
| `test_tasks`     | (beta) number of tasks used to run test case  | ESMX_TEST_TASKS        |


##### Build Types:

**`auto`** -
The ESMX build system searches for `CMakeLists.txt`, `Makefile`, and a build_script in order in the `source_dir` and uses the first build option it finds. If no build files are found then the ESMX build system searches for the CMake configuration file, fortran module, and libraries in the `install_prefix` directory but does not build the model. If no build option, CMake configuration, or libraries are found then the build fails.

**`cmake`** -
The ESMX build system searches for `CMakeLists.txt` in the `source_dir` and builds using CMake. Once built, the ESMX build system searches for the CMake configuration file and libraries in the `install_prefix` directory.

**`make`** -
The ESMX build system searches for `Makefile` in the `source_dir` and builds using Make without a target. If a specific target is desired then it can be configured using `build_args`. Once built, the ESMX build system searches for libraries and fortran modules in the `install_prefix` directory.

**`script`** -
The ESMX build system searches for a `build_script` in the `source_dir` and builds using this script. Once built, the ESMX build system searches for libraries and fortran modules in the `install_prefix` directory.

**`none`** -
The ESMX build system will not build the component. The ESMX build system searches for the CMake configuration file, fortran module, and libraries in the install_prefix directory.


#### Test Options (`tests` key)

This section contains a key for for each *test-name*, specifying test specific options.

| Option key       | Description / Value options                   | Default         |
| ---------------- | --------------------------------------------- | --------------- |
| `dir`            | (beta) directory used for test case data      | *None*          |
| `exe`            | (beta) executable used to run test case       | ESMX_TEST_EXE   |
| `tasks`          | (beta) number of tasks used to run test case  | ESMX_TEST_TASKS |

### ESMX Run Configuration

At startup, the `ESMX_EXE_NAME` executable checks the first command line argument for a filename. If no argument is provided then the filename defaults to `esmxRun.yaml`. This is the ESMX run-time configuration file in [YAML](https://yaml.org/) format, providing settings such as the list of components active at run-time, component attributes, the run sequence, as well as application level options.

An example `ESMX Run Configuration` file is given here:


```
ESMX:

  App:
    globalResourceControl:  true
    logKindFlag:            ESMF_LOGKIND_Multi
    logAppendFlag:          false
    logFlush:               true
    startTime:              2012-10-24T18:00:00
    stopTime:               2012-10-24T19:00:00

  Driver:
    componentList:          [ATM, OCN]
    attributes:
      Verbosity: low

    runSequence: |
      @900
        ATM -> OCN
        OCN -> ATM
        ATM
        OCN
      @

ATM:
  model:            Tawas     # model value is case insensitive to match Fortran
  ompNumThreads:    4
  attributes:
    Verbosity:  low
  petList:          [3, [2-0]]  # petList is list of scalars and lists.
                                # each list again can be of scalars and lists
                                # recursively.

OCN:
  model:            lumo
  petList:          [0-1, 3]
  attributes:
    Verbosity:  low
```

On the highest level, `ESMX Run Configuration` is expected to define the `ESMX` key, as well as a key for every component that is listed in the `componentList` found under the `Driver` level. The `ESMX` key is associated with a map containing the `App` and `Driver` keys. The `App` key must be present if `ESMX Run Configuration` is read by the `ESMX_EXE_NAME` executable, but is optional (and will be ignored) in case `esmfRun.yaml` is read by the `esmx_driver`.

#### ESMX/App Options

This section affects the application level.

**CAUTION:** Specifying any of the  `ESMF_RUNTIME_*` keys overrides the corresponding environment variables set in the user's environment.

| Option key                | Description / Value options                               | Default         |
| ------------------------- | --------------------------------------------------------- | --------------- |
| `startTime`               | string setting the application start time                 | *non-optional*  |
| `stopTime`                | string setting the application stop time                  | *non-optional*  |
| `globalResourceControl`   | enable/disable global resource control: `true` or `false` | `false`         |
| `logKindFlag`             | ESMF logging kind, see ESMF RefDoc for options            | `ESMF_LOGKIND_Multi_On_Error`|
| `logAppendFlag`           | enable/disable log append: `true` or `false`              | `true`          |
| `defaultLogFilename`      | name of the default ESMF log file (suffix if multi PET)   | `ESMF_LogFile`  |
| `defaultCalKind`          | ESMF calendar kind used by default, see ESMF RefDoc for options | `ESMF_CALKIND_GREGORIAN` |
| `logFlush`                | enable/disable log flush for each write: `true` or `false`| `false`         |
| `fieldDictionary`         | name of the NUOPC field dictionary file to be loaded      | *None*          |
| `ESMF_RUNTIME_PROFILE`    | enable/disable all profiling functions: `ON` or `OFF`     | `OFF`           |
| `ESMF_RUNTIME_PROFILE_OUTPUT` | output format; multiple can be selected: `TEXT` `SUMMARY` `BINARY` | `TEXT`      |
| `ESMF_RUNTIME_PROFILE_PETLIST`| limit profiling to an explicit list of PETs           | *all PETs*      |
| `ESMF_RUNTIME_TRACE`      | enable/disable all tracing functions: `ON` or `OFF`       | `OFF`           |
| `ESMF_RUNTIME_TRACE_CLOCK`| type of clock for events: `REALTIME` or `MONOTONIC` or `MONOTONIC_SYNC`  | `REALTIME`|
| `ESMF_RUNTIME_TRACE_PETLIST`| limit tracing to an explicit list of PETs               | *all PETs*      |
| `ESMF_RUNTIME_TRACE_COMPONENT`| enable/disable tracing of component events: `ON` or `OFF` | `ON`        |
| `ESMF_RUNTIME_TRACE_FLUSH`| frequency of event stream flushing to file: `DEFAULT` or `EAGER` | `DEFAULT`|
| `ESMF_RUNTIME_COMPLIANCECHECK`| enable/disable NUOPC compliance checking: `ON` or `OFF` with `DEPTH` | `OFF`|

#### ESMX/Driver Options

This section affects the driver level.

| Option key      | Description / Value options                                          | Default         |
| --------------- | -------------------------------------------------------------------- | --------------- |
| `componentList` | list of component labels, each matching a top level key in this file | *non-optional*  |
| `runSequence`   | block literal string defining the run sequence                       | *NUOPC default* |
| `attributes`    | map of key value pairs, each defining a driver attribute             | *None*          |

#### Component Label Options

This section affects the specific component instance.

| Option key            | Description / Value options                                           | Default         |
| --------------------- | --------------------------------------------------------------------- | --------------- |
| `model`               | string associating the instance with a *component-name* defined in `esmxBuild.yaml` | *non-optional*  |
| `petList`             | list of PETs on which the component executes                          | *None*          |
| `devList`             | list of DEVs (accelerator devices) to be associated with the component| *None*          |
| `ompNumThreads`       | setting of /NUOPC/Hint/PePerPet/MaxCount (see NUOPC ref doc)          | *None*          |
| `attributes`          | map of key value pairs, each defining a component attribute           | *None*          |
| *model specific yaml* | each model can define its own YAML section, e.g. with key value pairs, etc. | *None*          |

### Dynamically loading components from shared objects at run-time

There are two options recognized when specifying the value of the `model` field for a component in the `esmxRun.yaml` file:
- First, if the value specified is recognized as a *component-name* provided by any of the components built into `esmx` during build-time, as specified by `esmxBuild.yaml`, the respective component is accessed via its Fortran module.
- Second, if the value does not match a build-time dependency, it is assumed to correspond to a shared object instead. In that case the attempt is made to load the specified shared object at run-time, and to associate with the generic component label.

## The Unfied ESMX_Driver

The ESMX layer provides access to the `ESMX_Driver` via the public NUOPC Driver API. This means that `ESMX_Driver` can be plugged into a larger NUOPC application as a standard NUOPC component, or alternatively be accessed through the [External NUOPC Interface](https://earthsystemmodeling.org/docs/nightly/develop/NUOPC_refdoc/node3.html#SECTION00038000000000000000).
A good starting point to explore this feature is the [ESMX_ExternalDriverAPIProto](https://github.com/esmf-org/nuopc-app-prototypes/tree/develop/ESMX_ExternalDriverAPIProto) under the NUOPC prototype repository.

### Project integration

The typical situation where `ESMX_Driver` comes into play is where a user application needs to access a NUOPC based system that uses the unified ESMX driver. Assuming the user application uses CMake, integration of ESMX is straight forward. The critical piece required is to add `add_subdirectory()` in the application's `CMakeLists.txt` file to bring in the `${ESMF_ESMXDIR}/Driver` directory, and make the application dependent on target `esmx_driver`. An example for a very simple application is shown:

```
cmake_minimum_required(VERSION 3.22)

# Where to look for the local Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH "${ESMF_ESMXDIR}/Driver/cmake")

# Find ESMF
find_package(ESMF 8.5.0 MODULE REQUIRED)

# Set compilers consistent with ESMF
set(CMAKE_Fortran_COMPILER        "${ESMF_F90COMPILER}")
set(CMAKE_CXX_COMPILER            "${ESMF_CXXCOMPILER}")
set(CMAKE_C_COMPILER              "${ESMF_CCOMPILER}")

# Project
project(ExternalDriverAPIProto
        VERSION 1.0.0
        LANGUAGES Fortran CXX C
        )

# Add ESMX driver
add_subdirectory(${ESMF_ESMXDIR}/Driver ./ESMX_Driver)

# Create executable
add_executable(externalApp externalApp.F90)
target_include_directories(externalApp PUBLIC ${PROJECT_BINARY_DIR})
target_link_libraries(externalApp PUBLIC esmx_driver)

# Install executable
install(
  TARGETS externalApp
  EXPORT externalApp
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)
```

The applcation can then be built as typically via cmake commands, only requiring that the `ESMF_ESMXDIR` variable is passed in. It can be convenient to wrap the cmake commands into a GNU Makefile, accessing the `ESMF_ESMXDIR` variable through the `ESMFMKFILE` mechanism.

When executing cmake, the [ESMX_BUILD_FILE](#esmx-build-configuration) can be specified using `-DESMX_BUILD_FILE=<value>`. If ESMX_BUILD_FILE is not defined in the command line arguments then it will default to `esmxBuild.yaml`.

```
include $(ESMFMKFILE)

build/externalApp: externalApp.F90 esmxBuild.yaml
        cmake -S. -Bbuild -DESMF_ESMXDIR=$(ESMF_ESMXDIR) -DESMX_BUILD_FILE="<value>"
        cmake --build ./build
```

### esmxBuild.yaml and esmxRun.yaml

The `esmx_driver` target defined by the `add_subdirectory(${ESMF_ESMXDIR}/Driver ./ESMX_Driver)` has a build-time dependency on the ESMX_BUILD_FILE already discussed under the [ESMX Build Configuration section](#esmx-build-configuration). The identical file can be used when working on the `ESMX_Driver` level.

The run-time configuration needed by `ESMX_Driver` can either be supplied by the user application, or alternatively default to `esmxRun.yaml`. The following rules apply:
- `ESMX_Driver`, at the beginning of its `SetModelServices()` method checks whether the parent level has provided an `ESMF_Config` object by setting the `config` member on the `ESMX_Driver` component. If so, the provided `config` object is used. Otherwise `ESMX_Driver` itself creates `config` from file `esmxRun.yaml`.
- For the case where the `config` object was provided by the parent layer, `ESMX_Driver` does not ingest attributes from `config`. Instead the assumption is made that the parent layer sets the desired attributes on `ESMX_Driver`.
- For the case where the `config` object was loaded from `esmxRun.yaml` by `ESMX_Driver`, the driver ingests attributes from `config`, potentially overriding parent level settings.
- The `ESMX_component_list`, child component, and run sequence information is ingested from `config` as described under the [ESMX Run Configuration section](#esmx-run-configuration).
- If the parent level passes an `ESMF_Clock` object to `ESMX_Driver` during initialize, the driver uses it instead of looking for `startTime` and `stopTime` in `config`.
- For the case where a clock is provided by the parent layer, its `timeStep` is used as the *default* time step of the outer run sequence loop when using the `@*` syntax. If a specific time step is set in the run sequence with `@DT`, then `DT` must be a divisor of the `timeStep` provided by the parent clock.

## ESMX Test System (beta)

The ESMX layer includes a test system based on CTest. This system is still in beta and as it evolves features may be added or removed. When enabled the test system adds a suite of component and system level tests. All tests in the test suite can be executed via `ESMX_Builder -t`. If the `ESMX_Builder` is not used then tests can be added by setting `test: on` under `Application Options`. Tests can be manually executed from each directory in `test_dir` or through executing `ctest` in the `<CMAKE_BINARY_DIR>/Driver` directory.

## ESMX Components

ESMX includes a data component, which can be used for testing NUOPC caps. This component is known as [`ESMX_Data`](Comps/ESMX_Data).

## ESMX Software Dependencies

The ESMX layer has the following dependencies:
- **ESMF Library**: The ESMX layer is part of the ESMF repository. In order to use ESMX as described above, the ESMF library first needs to be built following the instructions for [Building ESMF](https://github.com/esmf-org/esmf#building-esmf).
- **CMake**: v3.22 or greater.
- **Python**: v3.5 or greater.
  - `python3` must be in `$PATH`.
  - `PyYaml` must be installed in the Python environment.

There are many ways to provide a suitable Python environment. One portable way based on [venv](https://docs.python.org/3/library/venv.html) is shown below.

```
python3 -m venv ESMXenv
source ESMXenv/bin/activate (for Bash)  or   source ESMXenv/bin/activate.csh (for Csh)
pip install pyyaml
... Environment ready for ESMX ...
deactivate
```
