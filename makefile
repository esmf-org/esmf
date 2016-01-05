# $Id$
#===============================================================================
#                            makefile
# 
# This is the primary makefile for building the Earth System Modeling Framework
# (ESMF).  
#===============================================================================

#-------------------------------------------------------------------------------
# The ESMF_DIR environment variable MUST be set by the user!
#-------------------------------------------------------------------------------
ifndef ESMF_DIR
$(error ESMF_DIR needs to be set to the top ESMF directory)
endif
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# All common rules and definitions are in common.mk.
#-------------------------------------------------------------------------------
include $(ESMF_DIR)/build/common.mk
# default target inherited from common.mk is 'lib'
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Include dependencies, if they exist.
-include $(LOCAL_DEPEND_FILE)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Things specific to framework build.
#-------------------------------------------------------------------------------
DIRS = src

CLEANDIRS = $(ESMF_LIBDIR) $(ESMF_MODDIR) $(ESMF_OBJDIR) $(ESMF_TESTDIR) \
  $(ESMF_EXDIR) $(ESMF_BUILD)/src/include $(ESMF_ETCDIR) $(ESMF_APPSDIR)
CLOBBERDIRS = $(ESMF_BUILD)/lib $(ESMF_BUILD)/mod $(ESMF_BUILD)/obj \
	      $(ESMF_BUILD)/test $(ESMF_BUILD)/quick_start \
              $(ESMF_BUILD)/release $(ESMF_BUILD)/examples \
              $(ESMF_BUILD)/doc \
              $(ESMF_BUILD)/apps
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Basic targets to build ESMF libraries.
#-------------------------------------------------------------------------------
# Define what building "all" means for the framework.  This needs to be
# after the include of common.mk, so it does not interfere with the
# definition of the default build rule.

all:  lib build_unit_tests build_examples build_system_tests


#-------------------------------------------------------------------------------
# Prints information about the system and version of ESMF being compiled.
#-------------------------------------------------------------------------------
script_info:
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo "Make version:"; $(MAKE) -v; echo ""
	-@echo "--------------------------------------------------------------"
	-@echo "Fortran Compiler version:"; $(ESMF_F90COMPILER_VERSION); echo ""
	-@echo "--------------------------------------------------------------"
	-@echo "C++ Compiler version:"; $(ESMF_CXXCOMPILER_VERSION); echo "" 
	-@echo "--------------------------------------------------------------"
	-@echo "Preprocessor version:"
	@$(ESMF_CPP) --version $(ESMF_DIR)/scripts/empty.C
	-@echo "" 
	-@echo "--------------------------------------------------------------"
	-@echo "ESMF_VERSION_STRING:    $(ESMF_VERSION_STRING)"
	-@echo "--------------------------------------------------------------"
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo " * User set ESMF environment variables *"
	$(shell $(ESMF_DIR)/scripts/envecho)
	-@cat envecho.out; rm -f envecho.out
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo " * ESMF environment variables *"
	-@echo "ESMF_DIR: $(ESMF_DIR)"
	-@if [ "$(ESMF_BUILD)" != "$(ESMF_DIR)" ] ; then \
	  echo "ESMF_BUILD:             $(ESMF_BUILD)" ; fi
	-@echo "ESMF_OS:                $(ESMF_OS)"
	-@echo "ESMF_MACHINE:           $(ESMF_MACHINE)"
	-@echo "ESMF_ABI:               $(ESMF_ABI)"
	-@echo "ESMF_COMPILER:          $(ESMF_COMPILER)"
	-@echo "ESMF_BOPT:              $(ESMF_BOPT)"
	-@if [ -n "$(ESMF_OPTLEVEL)" ] ; then \
	  echo "ESMF_OPTLEVEL:          $(ESMF_OPTLEVEL)" ; fi
	-@echo "ESMF_COMM:              $(ESMF_COMM)"
	-@echo "ESMF_SITE:              $(ESMF_SITE)"
	-@echo "ESMF_PTHREADS:          $(ESMF_PTHREADS)"
	-@echo "ESMF_OPENMP:            $(ESMF_OPENMP)"
	-@echo "ESMF_OPENACC:           $(ESMF_OPENACC)"
	-@echo "ESMF_ARRAY_LITE:        $(ESMF_ARRAY_LITE)"
	-@echo "ESMF_NO_INTEGER_1_BYTE: $(ESMF_NO_INTEGER_1_BYTE)"
	-@echo "ESMF_NO_INTEGER_2_BYTE: $(ESMF_NO_INTEGER_2_BYTE)"
	-@echo "ESMF_FORTRANSYMBOLS:    $(ESMF_FORTRANSYMBOLS)"
	-@echo "ESMF_DEFER_LIB_BUILD:   $(ESMF_DEFER_LIB_BUILD)"
	-@echo "ESMF_SHARED_LIB_BUILD:  $(ESMF_SHARED_LIB_BUILD)"
	-@echo "ESMF_TESTEXHAUSTIVE:    $(ESMF_TESTEXHAUSTIVE)"
	-@echo "ESMF_TESTWITHTHREADS:   $(ESMF_TESTWITHTHREADS)"
	-@echo "ESMF_TESTMPMD:          $(ESMF_TESTMPMD)"
	-@echo "ESMF_TESTSHAREDOBJ:     $(ESMF_TESTSHAREDOBJ)"
	-@echo "ESMF_TESTFORCEOPENMP:   $(ESMF_TESTFORCEOPENMP)"
	-@echo "ESMF_TESTFORCEOPENACC:  $(ESMF_TESTFORCEOPENACC)"
	-@echo "ESMF_TESTHARNESS_ARRAY: $(ESMF_TESTHARNESS_ARRAY)"
	-@echo "ESMF_TESTHARNESS_FIELD: $(ESMF_TESTHARNESS_FIELD)"
	-@echo "ESMF_MPIRUN:            $(ESMF_MPIRUN)"
	-@if [ "$(ESMF_TESTMPMD)" = "ON" ] ; then \
	  echo "ESMF_MPIMPMDRUN:        $(ESMF_MPIMPMDRUN)" ; fi
	-@if [ -n "$(ESMF_MPISCRIPTOPTIONS)" ] ; then \
	  echo "ESMF_MPISCRIPTOPTIONS:  $(ESMF_MPISCRIPTOPTIONS)" ; fi
	-@if [ -n "$(ESMF_MPIBATCHOPTIONS)" ] ; then \
	  echo "ESMF_MPIBATCHOPTIONS:   $(ESMF_MPIBATCHOPTIONS)" ; fi
	-@if [ -n "$(ESMF_MPILAUNCHOPTIONS)" ] ; then \
	  echo "ESMF_MPILAUNCHOPTIONS:  $(ESMF_MPILAUNCHOPTIONS)" ; fi
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo " * ESMF environment variables pointing to 3rd party software *"
	-@if [ -n "$(ESMF_MOAB)" ] ; then \
	  echo "ESMF_MOAB:              $(ESMF_MOAB)" ; \
	  if [ -n "$(ESMF_MOAB_INCLUDE)" ] ; then \
	    echo "ESMF_MOAB_INCLUDE:      $(ESMF_MOAB_INCLUDE)" ; \
          fi; \
	  if [ -n "$(ESMF_MOAB_LIBS)" ] ; then \
	    echo "ESMF_MOAB_LIBS:         $(ESMF_MOAB_LIBS)" ; \
          fi; \
	  if [ -n "$(ESMF_MOAB_LIBPATH)" ] ; then \
	    echo "ESMF_MOAB_LIBPATH:      $(ESMF_MOAB_LIBPATH)" ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_LAPACK)" ] ; then \
	  echo "ESMF_LAPACK:            $(ESMF_LAPACK)" ; \
	  if [ -n "$(ESMF_LAPACK_LIBS)" ] ; then \
	    echo "ESMF_LAPACK_LIBS:       $(ESMF_LAPACK_LIBS)" ; \
          fi; \
	  if [ -n "$(ESMF_LAPACK_LIBPATH)" ] ; then \
	    echo "ESMF_LAPACK_LIBPATH:    $(ESMF_LAPACK_LIBPATH)" ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_ACC_SOFTWARE_STACK)" ] ; then \
	  echo "ESMF_ACC_SOFTWARE_STACK:            $(ESMF_ACC_SOFTWARE_STACK)" ; \
	  if [ -n "$(ESMF_ACC_SOFTWARE_STACK_INCLUDE)" ] ; then \
	    echo "ESMF_ACC_SOFTWARE_STACK_INCLUDE:    $(ESMF_ACC_SOFTWARE_STACK_INCLUDE)" ; \
          fi; \
	  if [ -n "$(ESMF_ACC_SOFTWARE_STACK_LIBS)" ] ; then \
	    echo "ESMF_ACC_SOFTWARE_STACK_LIBS:       $(ESMF_ACC_SOFTWARE_STACK_LIBS)" ; \
          fi; \
	  if [ -n "$(ESMF_ACC_SOFTWARE_STACK_LIBPATH)" ] ; then \
	    echo "ESMF_ACC_SOFTWARE_STACK_LIBPATH:    $(ESMF_ACC_SOFTWARE_STACK_LIBPATH)" ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_NETCDF)" ] ; then \
	  echo "ESMF_NETCDF:            $(ESMF_NETCDF)" ; \
	  if [ -n "$(ESMF_NETCDF_INCLUDE)" ] ; then \
	    echo "ESMF_NETCDF_INCLUDE:    $(ESMF_NETCDF_INCLUDE)" ; \
          fi; \
	  if [ -n "$(ESMF_NETCDF_LIBS)" ] ; then \
	    echo "ESMF_NETCDF_LIBS:       $(ESMF_NETCDF_LIBS)" ; \
          fi; \
	  if [ -n "$(ESMF_NETCDF_LIBPATH)" ] ; then \
	    echo "ESMF_NETCDF_LIBPATH:    $(ESMF_NETCDF_LIBPATH)" ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_PNETCDF)" ] ; then \
	  echo "ESMF_PNETCDF:           $(ESMF_PNETCDF)" ; \
	  if [ -n "$(ESMF_PNETCDF_INCLUDE)" ] ; then \
	    echo "ESMF_PNETCDF_INCLUDE:   $(ESMF_PNETCDF_INCLUDE)" ; \
          fi; \
	  if [ -n "$(ESMF_PNETCDF_LIBS)" ] ; then \
	    echo "ESMF_PNETCDF_LIBS:      $(ESMF_PNETCDF_LIBS)" ; \
          fi; \
	  if [ -n "$(ESMF_PNETCDF_LIBPATH)" ] ; then \
	    echo "ESMF_PNETCDF_LIBPATH:   $(ESMF_PNETCDF_LIBPATH)" ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_XERCES)" ] ; then \
	  echo "ESMF_XERCES:            $(ESMF_XERCES)" ; \
	  if [ -n "$(ESMF_XERCES_INCLUDE)" ] ; then \
	    echo "ESMF_XERCES_INCLUDE:    $(ESMF_XERCES_INCLUDE)" ; \
          fi; \
	  if [ -n "$(ESMF_XERCES_LIBS)" ] ; then \
	    echo "ESMF_XERCES_LIBS:       $(ESMF_XERCES_LIBS)" ; \
          fi; \
	  if [ -n "$(ESMF_XERCES_LIBPATH)" ] ; then \
	    echo "ESMF_XERCES_LIBPATH:    $(ESMF_XERCES_LIBPATH)" ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_PIO)" ] ; then \
	  echo "ESMF_PIO:               $(ESMF_PIO)" ; \
	  if [ -n "$(ESMF_PIO_INCLUDE)" ] ; then \
	    echo "ESMF_PIO_INCLUDE:       $(ESMF_PIO_INCLUDE)" ; \
          fi; \
	  if [ -n "$(ESMF_PIO_LIBS)" ] ; then \
	    echo "ESMF_PIO_LIBS:          $(ESMF_PIO_LIBS)" ; \
          fi; \
	  if [ -n "$(ESMF_PIO_LIBPATH)" ] ; then \
	    echo "ESMF_PIO_LIBPATH:       $(ESMF_PIO_LIBPATH)" ; \
          fi; \
         fi
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo " * ESMF environment variables for final installation *"
	-@echo "ESMF_INSTALL_PREFIX:    $(ESMF_INSTALL_PREFIX)"
	-@echo "ESMF_INSTALL_HEADERDIR: $(ESMF_INSTALL_HEADERDIR)"
	-@echo "ESMF_INSTALL_MODDIR:    $(ESMF_INSTALL_MODDIR)"
	-@echo "ESMF_INSTALL_LIBDIR:    $(ESMF_INSTALL_LIBDIR)"
	-@echo "ESMF_INSTALL_BINDIR:    $(ESMF_INSTALL_BINDIR)"
	-@echo "ESMF_INSTALL_DOCDIR:    $(ESMF_INSTALL_DOCDIR)"
	-@echo " "
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo " * ESMF Benchmark directory and parameters *"
	-@echo "ESMF_BENCHMARK_PREFIX:    $(ESMF_BENCHMARK_PREFIX)"
	-@echo "ESMF_BENCHMARK_TOLERANCE: $(ESMF_BENCHMARK_TOLERANCE)"
	-@echo "ESMF_BENCHMARK_THRESHOLD_MSEC: $(ESMF_BENCHMARK_THRESHOLD_MSEC)"
	-@echo " "

#
info:   script_info
	-@echo "--------------------------------------------------------------"
	-@echo " * Compilers, Linkers, Flags, and Libraries *"
	-@echo "Location of the preprocessor:     " `which $(word 1, $(ESMF_CPP))`
	-@echo "Location of the Fortran compiler: " `which $(word 1, $(ESMF_F90COMPILER))`
	-@echo "Location of the Fortran linker:   " `which $(word 1, $(ESMF_F90LINKER))`
	-@echo "Location of the C++ compiler:     " `which $(word 1, $(ESMF_CXXCOMPILER))`
	-@echo "Location of the C++ linker:       " `which $(word 1, $(ESMF_CXXLINKER))`
	-@echo ""
	-@echo "Fortran compiler flags:"
	-@echo "ESMF_F90COMPILEOPTS: $(ESMF_F90COMPILEOPTS)"
	-@echo "ESMF_F90COMPILEPATHS: $(ESMF_F90COMPILEPATHS)"
	-@echo "ESMF_F90COMPILECPPFLAGS: $(ESMF_F90COMPILECPPFLAGS)"
	-@echo "ESMF_F90COMPILEFREECPP: $(ESMF_F90COMPILEFREECPP)"
	-@echo "ESMF_F90COMPILEFREENOCPP: $(ESMF_F90COMPILEFREENOCPP)"
	-@echo "ESMF_F90COMPILEFIXCPP: $(ESMF_F90COMPILEFIXCPP)"
	-@echo "ESMF_F90COMPILEFIXNOCPP: $(ESMF_F90COMPILEFIXNOCPP)"
	-@echo ""
	-@echo "Fortran linker flags:"
	-@echo "ESMF_F90LINKOPTS: $(ESMF_F90LINKOPTS)"
	-@echo "ESMF_F90LINKPATHS: $(ESMF_F90LINKPATHS)"
	-@echo "ESMF_F90LINKRPATHS: $(ESMF_F90LINKRPATHS)"
	-@echo "ESMF_F90LINKLIBS: $(ESMF_F90LINKLIBS)"
	-@echo "ESMF_F90ESMFLINKLIBS: $(ESMF_F90ESMFLINKLIBS)"
	-@echo ""
	-@echo "C++ compiler flags:"
	-@echo "ESMF_CXXCOMPILEOPTS: $(ESMF_CXXCOMPILEOPTS)"
	-@echo "ESMF_CXXCOMPILEPATHS: $(ESMF_CXXCOMPILEPATHS)"
	-@echo "ESMF_CXXCOMPILECPPFLAGS: $(ESMF_CXXCOMPILECPPFLAGS)"
	-@echo ""
	-@echo "C++ linker flags:"
	-@echo "ESMF_CXXLINKOPTS: $(ESMF_CXXLINKOPTS)"
	-@echo "ESMF_CXXLINKPATHS: $(ESMF_CXXLINKPATHS)"
	-@echo "ESMF_CXXLINKRPATHS: $(ESMF_CXXLINKRPATHS)"
	-@echo "ESMF_CXXLINKLIBS: $(ESMF_CXXLINKLIBS)"
	-@echo "ESMF_CXXESMFLINKLIBS: $(ESMF_CXXESMFLINKLIBS)"
	-@echo ""
ifneq ($(strip $(ESMF_SL_LIBS_TO_MAKE)),)
	-@echo "Shared library build:"
	-@echo "ESMF_SL_LIBS_TO_MAKE: $(ESMF_SL_LIBS_TO_MAKE)"
	-@echo "ESMF_SL_SUFFIX:       $(ESMF_SL_SUFFIX)"
	-@echo "ESMF_SL_LIBLINKER:    $(ESMF_SL_LIBLINKER)"
	-@echo "ESMF_SL_LIBOPTS:      $(ESMF_SL_LIBOPTS)"
	-@echo "ESMF_SL_LIBLIBS:      $(ESMF_SL_LIBLIBS)"
endif
	-@echo ""
	-@echo ""
	-@echo "--------------------------------------------------------------"
ifeq ($(ESMF_OS),MinGW)
	-@echo Compiling on `date` on `uname -n`
else
	-@echo Compiling on `date` on `hostname`
endif
	-@echo Machine characteristics: `uname -a`
	-@echo "=============================================================="
	-@echo " "
#
#
MKINFO = $(ESMF_LIBDIR)/esmf.mk
info_mk: chkdir_lib
	-@$(ESMF_RM) $(MKINFO)
	-@echo "# ESMF application makefile fragment" > $(MKINFO)
	-@echo "#" >> $(MKINFO)
	-@echo "# Use the following ESMF_ variables to compile and link" >> $(MKINFO)
	-@echo "# your ESMF application against this ESMF build." >> $(MKINFO)
	-@echo "#" >> $(MKINFO)
	-@echo "# !!! VERY IMPORTANT: If the location of this ESMF build is   !!!" >> $(MKINFO)
	-@echo "# !!! changed, e.g. libesmf.a is copied to another directory, !!!" >> $(MKINFO)
	-@echo "# !!! this file - esmf.mk - must be edited to adjust to the   !!!" >> $(MKINFO)
	-@echo "# !!! correct new path                                        !!!" >> $(MKINFO)
	-@echo "#" >> $(MKINFO)
	-@echo "# Please see end of file for options used on this ESMF build" >> $(MKINFO)
	-@echo "#" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_VERSION_STRING=$(ESMF_VERSION_STRING)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR)" >> $(MKINFO)
	-@echo "ESMF_VERSION_MINOR=$(ESMF_VERSION_MINOR)" >> $(MKINFO)
	-@echo "ESMF_VERSION_REVISION=$(ESMF_VERSION_REVISION)" >> $(MKINFO)
	-@echo "ESMF_VERSION_PATCHLEVEL=$(ESMF_VERSION_PATCHLEVEL)" >> $(MKINFO)
	-@echo "ESMF_VERSION_PUBLIC=$(ESMF_VERSION_PUBLIC)" >> $(MKINFO)
	-@echo "ESMF_VERSION_BETASNAPSHOT=$(ESMF_VERSION_BETASNAPSHOT)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_APPSDIR=$(ESMF_APPSDIR)" >> $(MKINFO)
	-@echo "ESMF_LIBSDIR=$(ESMF_LIBDIR)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_F90COMPILER=$(ESMF_F90COMPILER)" >> $(MKINFO)
	-@echo "ESMF_F90LINKER=$(ESMF_F90LINKER)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEOPTS=$(ESMF_F90COMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEPATHS=$(ESMF_F90COMPILEPATHS)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILECPPFLAGS=$(ESMF_F90COMPILECPPFLAGS)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEFREECPP=$(ESMF_F90COMPILEFREECPP)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEFREENOCPP=$(ESMF_F90COMPILEFREENOCPP)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEFIXCPP=$(ESMF_F90COMPILEFIXCPP)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEFIXNOCPP=$(ESMF_F90COMPILEFIXNOCPP)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_F90LINKOPTS=$(ESMF_F90LINKOPTS)" >> $(MKINFO)
	-@echo "ESMF_F90LINKPATHS=$(ESMF_F90LINKPATHS)" >> $(MKINFO)
	-@echo "ESMF_F90LINKRPATHS=$(ESMF_F90LINKRPATHS)" >> $(MKINFO)
	-@echo "ESMF_F90LINKLIBS=$(ESMF_F90LINKLIBS)" >> $(MKINFO)
	-@echo "ESMF_F90ESMFLINKLIBS=$(ESMF_F90ESMFLINKLIBS)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_CXXCOMPILER=$(ESMF_CXXCOMPILER)" >> $(MKINFO)
	-@echo "ESMF_CXXLINKER=$(ESMF_CXXLINKER)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_CXXCOMPILEOPTS=$(ESMF_CXXCOMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_CXXCOMPILEPATHS=$(ESMF_CXXCOMPILEPATHS)" >> $(MKINFO)
	-@echo "ESMF_CXXCOMPILECPPFLAGS=$(ESMF_CXXCOMPILECPPFLAGS)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_CXXLINKOPTS=$(ESMF_CXXLINKOPTS)" >> $(MKINFO)
	-@echo "ESMF_CXXLINKPATHS=$(ESMF_CXXLINKPATHS)" >> $(MKINFO)
	-@echo "ESMF_CXXLINKRPATHS=$(ESMF_CXXLINKRPATHS)" >> $(MKINFO)
	-@echo "ESMF_CXXLINKLIBS=$(ESMF_CXXLINKLIBS)" >> $(MKINFO)
	-@echo "ESMF_CXXESMFLINKLIBS=$(ESMF_CXXESMFLINKLIBS)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_SO_F90COMPILEOPTS=$(ESMF_SO_F90COMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_SO_F90LINKOPTS=$(ESMF_SO_F90LINKOPTS)" >> $(MKINFO)
	-@echo "ESMF_SO_F90LINKOPTSEXE=$(ESMF_SO_F90LINKOPTSEXE)" >> $(MKINFO)
	-@echo "ESMF_SO_CXXCOMPILEOPTS=$(ESMF_SO_CXXCOMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_SO_CXXLINKOPTS=$(ESMF_SO_CXXLINKOPTS)" >> $(MKINFO)
	-@echo "ESMF_SO_CXXLINKOPTSEXE=$(ESMF_SO_CXXLINKOPTSEXE)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_OPENMP_F90COMPILEOPTS=$(ESMF_OPENMP_F90COMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_OPENMP_F90LINKOPTS=$(ESMF_OPENMP_F90LINKOPTS)" >> $(MKINFO)
	-@echo "ESMF_OPENMP_CXXCOMPILEOPTS=$(ESMF_OPENMP_CXXCOMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_OPENMP_CXXLINKOPTS=$(ESMF_OPENMP_CXXLINKOPTS)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_OPENACC_F90COMPILEOPTS=$(ESMF_OPENACC_F90COMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_OPENACC_F90LINKOPTS=$(ESMF_OPENACC_F90LINKOPTS)" >> $(MKINFO)
	-@echo "ESMF_OPENACC_CXXCOMPILEOPTS=$(ESMF_OPENACC_CXXCOMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_OPENACC_CXXLINKOPTS=$(ESMF_OPENACC_CXXLINKOPTS)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "# Internal ESMF variables, do NOT depend on these!" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_INTERNAL_DIR=$(ESMF_DIR)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "#" >> $(MKINFO)
	-@echo "# !!! The following options were used on this ESMF build !!!" >> $(MKINFO)
	-@echo "#" >> $(MKINFO)
	-@echo "# ESMF_DIR: $(ESMF_DIR)" >> $(MKINFO)
	-@if [ "$(ESMF_BUILD)" != "$(ESMF_DIR)" ] ; then \
	  echo "# ESMF_BUILD: $(ESMF_BUILD)" >> $(MKINFO) ; fi
	-@echo "# ESMF_OS: $(ESMF_OS)" >> $(MKINFO)
	-@echo "# ESMF_MACHINE: $(ESMF_MACHINE)" >> $(MKINFO)
	-@echo "# ESMF_ABI: $(ESMF_ABI)" >> $(MKINFO)
	-@echo "# ESMF_COMPILER: $(ESMF_COMPILER)" >> $(MKINFO)
	-@echo "# ESMF_BOPT: $(ESMF_BOPT)" >> $(MKINFO)
	-@if [ -n "$(ESMF_OPTLEVEL)" ] ; then \
	  echo "# ESMF_OPTLEVEL: $(ESMF_OPTLEVEL)" >> $(MKINFO) ; fi
	-@echo "# ESMF_COMM: $(ESMF_COMM)" >> $(MKINFO)
	-@echo "# ESMF_SITE: $(ESMF_SITE)" >> $(MKINFO)
	-@echo "# ESMF_PTHREADS: $(ESMF_PTHREADS)" >> $(MKINFO)
	-@echo "# ESMF_OPENMP: $(ESMF_OPENMP)" >> $(MKINFO)
	-@echo "# ESMF_OPENACC: $(ESMF_OPENACC)" >> $(MKINFO)
	-@echo "# ESMF_ARRAY_LITE: $(ESMF_ARRAY_LITE)" >> $(MKINFO)
	-@echo "# ESMF_NO_INTEGER_1_BYTE: $(ESMF_NO_INTEGER_1_BYTE)" >> $(MKINFO)
	-@echo "# ESMF_NO_INTEGER_2_BYTE: $(ESMF_NO_INTEGER_2_BYTE)" >> $(MKINFO)
	-@echo "# ESMF_FORTRANSYMBOLS: $(ESMF_FORTRANSYMBOLS)" >> $(MKINFO)
	-@echo "# ESMF_DEFER_LIB_BUILD: $(ESMF_DEFER_LIB_BUILD)" >> $(MKINFO)
	-@echo "# ESMF_SHARED_LIB_BUILD: $(ESMF_SHARED_LIB_BUILD)" >> $(MKINFO)
	-@echo "# " >> $(MKINFO)
	-@echo "# ESMF environment variables pointing to 3rd party software:" >> $(MKINFO)
	-@if [ -n "$(ESMF_MOAB)" ] ; then \
	  echo "# ESMF_MOAB:              $(ESMF_MOAB)" >> $(MKINFO) ; \
	  if [ -n "$(ESMF_MOAB_INCLUDE)" ] ; then \
	    echo "# ESMF_MOAB_INCLUDE:      $(ESMF_MOAB_INCLUDE)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_MOAB_LIBS)" ] ; then \
	    echo "# ESMF_MOAB_LIBS:         $(ESMF_MOAB_LIBS)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_MOAB_LIBPATH)" ] ; then \
	    echo "# ESMF_MOAB_LIBPATH:      $(ESMF_MOAB_LIBPATH)" >> $(MKINFO) ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_LAPACK)" ] ; then \
	  echo "# ESMF_LAPACK:            $(ESMF_LAPACK)" >> $(MKINFO) ; \
	  if [ -n "$(ESMF_LAPACK_LIBS)" ] ; then \
	    echo "# ESMF_LAPACK_LIBS:       $(ESMF_LAPACK_LIBS)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_LAPACK_LIBPATH)" ] ; then \
	    echo "# ESMF_LAPACK_LIBPATH:    $(ESMF_LAPACK_LIBPATH)" >> $(MKINFO) ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_ACC_SOFTWARE_STACK)" ] ; then \
	  echo "#ESMF_ACC_SOFTWARE_STACK:            $(ESMF_ACC_SOFTWARE_STACK)" >> $(MKINFO) ; \
	  if [ -n "$(ESMF_ACC_SOFTWARE_STACK_INCLUDE)" ] ; then \
	    echo "# ESMF_ACC_SOFTWARE_STACK_INCLUDE:    $(ESMF_ACC_SOFTWARE_STACK_INCLUDE)" >> $(MKINFO); \
          fi; \
	  if [ -n "$(ESMF_ACC_SOFTWARE_STACK_LIBS)" ] ; then \
	    echo "# ESMF_ACC_SOFTWARE_STACK_LIBS:       $(ESMF_ACC_SOFTWARE_STACK_LIBS)" >> $(MKINFO); \
          fi; \
	  if [ -n "$(ESMF_ACC_SOFTWARE_STACK_LIBPATH)" ] ; then \
	    echo "# ESMF_ACC_SOFTWARE_STACK_LIBPATH:    $(ESMF_ACC_SOFTWARE_STACK_LIBPATH)" >> $(MKINFO); \
          fi; \
         fi
	-@if [ -n "$(ESMF_NETCDF)" ] ; then \
	  echo "# ESMF_NETCDF:            $(ESMF_NETCDF)" >> $(MKINFO) ; \
	  if [ -n "$(ESMF_NETCDF_INCLUDE)" ] ; then \
	    echo "# ESMF_NETCDF_INCLUDE:    $(ESMF_NETCDF_INCLUDE)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_NETCDF_LIBS)" ] ; then \
	    echo "# ESMF_NETCDF_LIBS:       $(ESMF_NETCDF_LIBS)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_NETCDF_LIBPATH)" ] ; then \
	    echo "# ESMF_NETCDF_LIBPATH:    $(ESMF_NETCDF_LIBPATH)" >> $(MKINFO) ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_PNETCDF)" ] ; then \
	  echo "# ESMF_PNETCDF:           $(ESMF_PNETCDF)" >> $(MKINFO) ; \
	  if [ -n "$(ESMF_PNETCDF_INCLUDE)" ] ; then \
	    echo "# ESMF_PNETCDF_INCLUDE:   $(ESMF_PNETCDF_INCLUDE)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_PNETCDF_LIBS)" ] ; then \
	    echo "# ESMF_PNETCDF_LIBS:      $(ESMF_PNETCDF_LIBS)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_PNETCDF_LIBPATH)" ] ; then \
	    echo "# ESMF_PNETCDF_LIBPATH:   $(ESMF_PNETCDF_LIBPATH)" >> $(MKINFO) ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_XERCES)" ] ; then \
	  echo "# ESMF_XERCES:            $(ESMF_XERCES)" >> $(MKINFO) ; \
	  if [ -n "$(ESMF_XERCES_INCLUDE)" ] ; then \
	    echo "# ESMF_XERCES_INCLUDE:    $(ESMF_XERCES_INCLUDE)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_XERCES_LIBS)" ] ; then \
	    echo "# ESMF_XERCES_LIBS:       $(ESMF_XERCES_LIBS)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_XERCES_LIBPATH)" ] ; then \
	    echo "# ESMF_XERCES_LIBPATH:    $(ESMF_XERCES_LIBPATH)" >> $(MKINFO) ; \
          fi; \
         fi
	-@if [ -n "$(ESMF_PIO)" ] ; then \
	  echo "# ESMF_PIO:               $(ESMF_PIO)" >> $(MKINFO) ; \
	  if [ -n "$(ESMF_PIO_INCLUDE)" ] ; then \
	    echo "# ESMF_PIO_INCLUDE:       $(ESMF_PIO_INCLUDE)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_PIO_LIBS)" ] ; then \
	    echo "# ESMF_PIO_LIBS:          $(ESMF_PIO_LIBS)" >> $(MKINFO) ; \
          fi; \
	  if [ -n "$(ESMF_PIO_LIBPATH)" ] ; then \
	    echo "# ESMF_PIO_LIBPATH:       $(ESMF_PIO_LIBPATH)" >> $(MKINFO) ; \
          fi; \
         fi

# Rewrite esmf.mk during installation to ensure correct installation paths are encoded
install_info_mk:
	$(MAKE) info_mk ESMF_APPSDIR=$(ESMF_INSTALL_BINDIR_ABSPATH) ESMF_LDIR=$(ESMF_INSTALL_LIBDIR_ABSPATH) ESMF_LIBDIR=$(ESMF_INSTALL_LIBDIR_ABSPATH) ESMF_MODDIR=$(ESMF_INSTALL_MODDIR_ABSPATH) ESMF_INCDIR=$(ESMF_INSTALL_HEADERDIR_ABSPATH)

# Relink apps during installation to ensure correct shared library location is encoded
install_apps:
	mkdir -p $(ESMF_INSTALL_BINDIR_ABSPATH)
	$(MAKE) build_apps ESMF_APPSDIR=$(ESMF_INSTALL_BINDIR_ABSPATH) ESMF_LDIR=$(ESMF_INSTALL_LIBDIR_ABSPATH) ESMF_LIBDIR=$(ESMF_INSTALL_LIBDIR_ABSPATH) ESMF_MODDIR=$(ESMF_INSTALL_MODDIR_ABSPATH)

# Ranlib on the libraries
ranlib:
	$(ESMF_RANLIB) $(wildcard $(ESMF_LIBDIR)/lib*.a)

# Deletes ESMF libraries
deletelibs: chkopts_basic
	-$(ESMF_RM) $(wildcard $(ESMF_LIBDIR)/lib*.*)


# ------------------------------------------------------------------
# All remaining actions are intended for ESMF developers only.
# ESMF users should not generally need to use these commands.


BUILDFILES = build/* build_config/*/*

DOCS	   = build/README build/doc/*

SCRIPTS    = 

# ------------------------------------------------------------------
# INSTALL target
install:
	-@echo " "
	-@echo "Installing ESMF:"
	-@echo " "
	mkdir -p $(ESMF_INSTALL_HEADERDIR_ABSPATH)
	cp -f $(ESMF_BUILD)/src/include/ESMC.h $(ESMF_INSTALL_HEADERDIR_ABSPATH)
	cp -f $(ESMF_BUILD)/src/include/ESMC_*.h $(ESMF_INSTALL_HEADERDIR_ABSPATH)
	cp -f $(ESMF_DIR)/build_config/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_SITE)/ESMC_Conf.h $(ESMF_INSTALL_HEADERDIR_ABSPATH)
	mkdir -p $(ESMF_INSTALL_MODDIR_ABSPATH)
	cp -f $(ESMF_MODDIR)/*.mod $(ESMF_INSTALL_MODDIR_ABSPATH)
	mkdir -p $(ESMF_INSTALL_LIBDIR_ABSPATH)
	cp -f $(ESMF_LIBDIR)/lib*.* $(ESMF_INSTALL_LIBDIR_ABSPATH)
ifneq ($(ESMF_OS),Cygwin)
	$(ESMF_RANLIB) $(ESMF_INSTALL_LIBDIR_ABSPATH)/lib*.a
endif
	$(MAKE) install_apps
	mkdir -p $(ESMF_INSTALL_DOCDIR_ABSPATH)
	@if [ -d $(ESMF_DOCDIR) ]; then \
	if [ `ls $(ESMF_DOCDIR)`foo != "foo" ]; then \
	cp -rf $(ESMF_DOCDIR)/* $(ESMF_INSTALL_DOCDIR_ABSPATH); \
	fi; \
        fi
	$(MAKE) install_info_mk
	-@echo " "
	-@echo "ESMF installation complete."
	-@echo " "
        
# ------------------------------------------------------------------
# INSTALLCHECK target
installcheck:
	-@echo " "
	-@echo "Checking ESMF installation:"
	-@echo " "
	cd $(ESMF_DIR)/src/installcheck ;\
	$(MAKE) clean
	cd $(ESMF_DIR)/src/installcheck ;\
	$(MAKE)
	cd $(ESMF_DIR)/src/installcheck ;\
        $(MAKE) check
	-@echo " "
	-@echo "ESMF installation check complete."
	-@echo " "

# ------------------------------------------------------------------
# Add dummy rules here to avoid gnumake trying to remake the actual
# makefiles themselves; this might not be much of an overhead but 
# since we call make so many times recursively and it does the makefile
# remake rule check on each invocation of make, it effectively renders
# gmake -d (debug mode) almost unreadable.  This cuts the remake rule
# output down immensely.  nsc 05nov04

GNUmakefile:
	@echo ;

makefile:
	@echo ;

$(ESMF_DIR)/makefile:
	@echo ;

$(ESMF_DIR)/build/common.mk:
	@echo ;

$(ESMF_DIR)/build_config/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_SITE)/build_rules.mk:
	@echo ;

