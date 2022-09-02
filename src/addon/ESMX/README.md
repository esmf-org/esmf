# ESMX

ESMX is the **E**arth **S**ystem **M**odel e**X**ecutable layer.

The ESMX layer is built on top of ESMF and NUOPC. ESMX user interfaces are typically implemented through configuration files.

The idea is to make it as simple as possible for a user to build, run, and test NUOPC based systems. The approach is supposed to be the same whether applied to a single component, or a fully coupled system of NUOPC-compliant components.

ESMX tries to unify, provide, and maintain those parts of NUOPC-based modeling systems that are common across most such implementations. This includes the top level application and driver code, parts of the build infrastructure, and tools to manage run configurations.

The objectives of ESMX are:
 - **Simplification** of standing up new NUOPC-based systems.
 - **Reduction** of maintenance cost for established NUOPC-based systems.
 - **Improved** alignment and interoperability between different NUOPC-based systems. (Configuration files, procedures, etc.)
 - **Faster and more coordinated** role out of new NUOPC/ESMF features.

## The Unified Driver Executable

One of the main features provided by ESMX is the *unified driver executable*. A good starting point to explore this feature is the [ESMX_AtmOcnProto](https://github.com/esmf-org/nuopc-app-prototypes/tree/develop/ESMX_AtmOcnProto) under the NUOPC prototype repository.

The default name of the unified driver executable built by ESMX is lower case `esmx`. This will be used for the remainder of this section to refer to the unfied driver executable.

### Project integration

A build target for `esmx` needs to be added to the projects build system. ESMX provides a CMake based approach that makes this easy. A simple target based on GNU Make looks like this:

    include $(ESMFMKFILE)

    esmx: esmxBuild.yaml
	    cmake -H$(ESMF_ESMXDIR) -Bbuild
	    cmake --build ./build

The `ESMF_ESMXDIR` variable used is defined by the ESMF installation through the `esmf.mk` file included in the first line.
A successful execution of this target will result in the unfied driver executable as `build/esmx`.

### esmxBuild.yaml

As shown above, building `esmx` has a dependency on file `esmxBuild.yaml`. This is a yaml file with a very simple format. An example is given here:

    components:

      tawas:
        config:       TaWaS/tawas.cmake
        fort_module:  tawas

      lumo:
        config:       Lumo/lumo.cmake

In this example two components are built into `esmx` explicitly. (Read about dynamically loading components from shared objects at run-time later.)

Each component is given a name, here `tawas` and `lumo`. Components will be referred to by this *component-name* in the run-time configuration (esmxRun.config) discussed below.

Each component must define the `config` key, specifying a file that can be included by the CMake based `esmx` build. This file must specify three CMake elements:
- `add_library(component-name ... )`
- `set_target_properties(component-name ... )`
- `target_link_libraries(esmx PRIVATE component-name)`

Here *component-name* is the name under which the component was defined in the `esmxBuild.yaml` file, here `tawas` or `lumo`. 

Further, a component can optionally provide the `fort_module` key. This explicitly specifys the name of the Fortran module that provides the entry points into the respective NUOPC component. By default the *component-name* is used.

### esmxRun.config

The esmxRun.config file needs to be located under the run directory from where the `esmx` executable is launched. It is read by `esmx` during startup. It specifies a few global ESMF and ESMX level settings, the list of components accessed during this run, details about the components, and finally the run sequence.

    logKindFlag:            ESMF_LOGKIND_MULTI
    globalResourceControl:  .true.
    
    ESMX_log_flush:        .true.
    ESMX_field_dictionary: ./fd.yaml

    ESMX_component_list:   ATM OCN
    ESMX_attributes::
      Verbosity = high
    ::
    
    ATM_model:            tawas
    ATM_omp_num_threads:  4
    ATM_attributes::
      Verbosity = high
    ::
    
    OCN_model:            lumo
    OCN_petlist:          1 3
    OCN_attributes::
      Verbosity = high
    ::
    
    startTime:  2012-10-24T18:00:00
    stopTime:   2012-10-24T19:00:00
    
    runSeq::
      @900
        ATM -> OCN
        OCN -> ATM
        ATM
        OCN
      @
    ::

The first two fields, `logKindFlag` and `globalResourceControl`, are examples of ESMF level configuration options. They are ingested by the [ESMF_Initialize()](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc/node4.html#SECTION04024100000000000000) method (called by `esmx`) as documented in the [ESMF Reference Manual](https://earthsystemmodeling.org/docs/nightly/develop/ESMF_refdoc/ESMF_refdoc.html).

Fields starting with `ESMX_` are defined by the ESMX unified driver executable. Most important on this level is `ESMX_component_list`. This field defines the generic components participating in the execution. There is no restriction what labels can be used for the generic components, however, `ATM` and `OCN` are typical labels for "atmosphere" and "ocean", respectively. Note that the run sequence under `runSeq`, toward the end of the file, is defined in terms of the generic component labels introduced by `ESMX_component_list`.

Each generic component label must be associated with an actual component model through the `XXX_model` field, where `XXX` is replaced by the actual generic component label, e.g. `ATM`, `OCN`, etc. The association is made by specifying the *component-name* used in the `esmxBuild.yaml` file discussed earlier. The options in the example are `tawas` and `lumo`.

Finally the `startTime` and `stopTime` fields set the start and stop time of the run, respectively. The `runSeq` field defines the run sequence.

### Dynamically loading components from shared objects at run-time

There are two options recognized when specifying the value of the `XXX_model` field:
- First, if the value specified is recognized as a *component-name* provided by any of the components built into `esmx` during build-time, as specified by `esmxBuild.yaml`, the respective component is accessed via its Fortran module.
- Second, if the value does not match a build-time dependency, it is assumed to correspond to a shared object. In that case the attempt is made to load the specified shared object at run-time, and to associate with the generic component label.

