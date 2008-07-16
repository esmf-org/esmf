# $Id: makefile,v 1.83.2.4 2008/07/16 00:19:06 theurich Exp $
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
-include $(ESMF_DIR)/$(LOCDIR)/makefile.dep
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Things specific to framework build.
#-------------------------------------------------------------------------------
DIRS = src

CLEANDIRS = $(ESMF_LIBDIR) $(ESMF_MODDIR) $(ESMF_TESTDIR) $(ESMF_EXDIR) \
	    $(ESMF_BUILD)/src/include
CLOBBERDIRS = $(ESMF_BUILD)/lib $(ESMF_BUILD)/mod \
	      $(ESMF_BUILD)/test $(ESMF_BUILD)/quick_start \
              $(ESMF_BUILD)/release $(ESMF_BUILD)/examples \
              $(ESMF_BUILD)/doc
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Basic targets to build ESMF libraries.
#-------------------------------------------------------------------------------
# Define what building "all" means for the framework.  This needs to be
# after the include of common.mk, so it does not interfere with the
# definition of the default build rule.

all:  lib build_unit_tests build_examples build_system_tests build_demos


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
	@$(ESMF_CPP) --version $(ESMF_DIR)/scpripts/empty.C
	-@echo "" 
	-@echo "--------------------------------------------------------------"
	-@if [ -f $(ESMF_DIR)/src/Infrastructure/Util/include/ESMC_Macros.h ] ; then \
	  fgrep ESMF_VERSION_STRING $(ESMF_DIR)/src/Infrastructure/Util/include/ESMC_Macros.h | $(ESMF_SED) "s/^#define //" ; fi
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
	-@echo "ESMF_ARRAY_LITE:        $(ESMF_ARRAY_LITE)"
	-@echo "ESMF_NO_INTEGER_1_BYTE: $(ESMF_NO_INTEGER_1_BYTE)"
	-@echo "ESMF_NO_INTEGER_2_BYTE: $(ESMF_NO_INTEGER_2_BYTE)"
	-@echo "ESMF_FORTRANSYMBOLS:    $(ESMF_FORTRANSYMBOLS)"
	-@echo "ESMF_TESTEXHAUSTIVE:    $(ESMF_TESTEXHAUSTIVE)"
	-@echo "ESMF_TESTWITHTHREADS:   $(ESMF_TESTWITHTHREADS)"
	-@echo "ESMF_TESTMPMD:          $(ESMF_TESTMPMD)"
	-@echo "ESMF_MPIRUN:            $(ESMF_MPIRUN)"
	-@if [ -n "$(ESMF_MPISCRIPTOPTIONS)" ] ; then \
	  echo "ESMF_MPISCRIPTOPTIONS:  $(ESMF_MPISCRIPTOPTIONS)" ; fi
	-@if [ -n "$(ESMF_MPIBATCHOPTIONS)" ] ; then \
	  echo "ESMF_MPIBATCHOPTIONS:   $(ESMF_MPIBATCHOPTIONS)" ; fi
	-@if [ -n "$(ESMF_MPILAUNCHOPTIONS)" ] ; then \
	  echo "ESMF_MPILAUNCHOPTIONS:  $(ESMF_MPILAUNCHOPTIONS)" ; fi
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo " * ESMF environment variables pointing to 3rd party software *"
	-@if [ -n "$(ESMF_NETCDF_INCLUDE)" ] ; then \
	  echo "ESMF_NETCDF_INCLUDE: $(ESMF_NETCDF_INCLUDE)" ; fi
	-@if [ -n "$(ESMF_NETCDF_LIBPATH)" ] ; then \
	  echo "ESMF_NETCDF_LIBPATH: $(ESMF_NETCDF_LIBPATH)" ; fi
	-@echo " "
	-@echo "--------------------------------------------------------------"
	-@echo " * ESMF environment variables for final installation *"
	-@echo "ESMF_INSTALL_PREFIX:    $(ESMF_INSTALL_PREFIX)"
	-@echo "ESMF_INSTALL_HEADERDIR: $(ESMF_INSTALL_HEADERDIR)"
	-@echo "ESMF_INSTALL_MODDIR:    $(ESMF_INSTALL_MODDIR)"
	-@echo "ESMF_INSTALL_LIBDIR:    $(ESMF_INSTALL_LIBDIR)"
	-@echo "ESMF_INSTALL_DOCDIR:    $(ESMF_INSTALL_DOCDIR)"
	-@echo " "
#
info:   script_info
	-@echo " "
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
	-@echo ""
	-@echo "--------------------------------------------------------------"
	-@echo Compiling on `date` on `hostname`
	-@echo Machine characteristics: `uname -a`
	-@echo "=============================================================="
	-@echo " "
#
#
MKINFO = $(ESMF_LIBDIR)/esmf.mk
info_mk:
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
	-@echo "ESMF_F90COMPILER=$(ESMF_F90COMPILER)" >> $(MKINFO)
	-@echo "ESMF_F90LINKER=$(ESMF_F90LINKER)" >> $(MKINFO)
	-@echo "" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEOPTS=$(ESMF_F90COMPILEOPTS)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILEPATHS=$(ESMF_F90COMPILEPATHS)" >> $(MKINFO)
	-@echo "ESMF_F90COMPILECPPFLAGS=$(ESMF_F90COMPILECPPFLAGS)" >> $(MKINFO)
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
	-@echo "# ESMF_ARRAY_LITE: $(ESMF_ARRAY_LITE)" >> $(MKINFO)
	-@echo "# ESMF_NO_INTEGER_1_BYTE: $(ESMF_NO_INTEGER_1_BYTE)" >> $(MKINFO)
	-@echo "# ESMF_NO_INTEGER_2_BYTE: $(ESMF_NO_INTEGER_2_BYTE)" >> $(MKINFO)
	-@echo "# ESMF_FORTRANSYMBOLS: $(ESMF_FORTRANSYMBOLS)" >> $(MKINFO)
	-@echo "# ESMF_TESTEXHAUSTIVE: $(ESMF_TESTEXHAUSTIVE)" >> $(MKINFO)
	-@echo "# ESMF_TESTWITHTHREADS: $(ESMF_TESTWITHTHREADS)" >> $(MKINFO)
	-@echo "# ESMF_TESTMPMD: $(ESMF_TESTMPMD)" >> $(MKINFO)
	-@echo "# " >> $(MKINFO)
	-@echo "# ESMF environment variables pointing to 3rd party software:" >> $(MKINFO)
	-@if [ -n "$(ESMF_NETCDF_INCLUDE)" ] ; then \
	  echo "# ESMF_NETCDF_INCLUDE: $(ESMF_NETCDF_INCLUDE)" >> $(MKINFO) ; fi
	-@if [ -n "$(ESMF_NETCDF_LIBPATH)" ] ; then \
	  echo "# ESMF_NETCDF_LIBPATH: $(ESMF_NETCDF_LIBPATH)" >> $(MKINFO) ; fi

install_info_mk:
	$(MAKE) info_mk ESMF_LDIR=$(ESMF_INSTALL_LIBDIR_ABSPATH) ESMF_LIBDIR=$(ESMF_INSTALL_LIBDIR_ABSPATH) ESMF_MODDIR=$(ESMF_INSTALL_MODDIR_ABSPATH) ESMF_INCDIR=$(ESMF_INSTALL_HEADERDIR_ABSPATH)

# Ranlib on the libraries
ranlib:
	$(ESMF_RANLIB) $(ESMF_LIBDIR)/*.a

# Deletes ESMF libraries
deletelibs: chkopts_basic
	-$(ESMF_RM) $(ESMF_LIBDIR)/*

# ESMF_COUPLED_FLOW/demo target.
ESMF_COUPLED_FLOW: chkopts build_libs chkdir_tests
	cd src/demos/coupled_flow ;\
	$(MAKE) BOPT=$(BOPT) demos

# ESMF_COUPLED_FLOW_uni/demo target.
ESMF_COUPLED_FLOW_uni: chkopts build_libs chkdir_tests
	cd src/demos/coupled_flow ;\
	$(MAKE) BOPT=$(BOPT) demos_uni


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
	cp -f $(ESMF_BUILD)/src/include/*.h $(ESMF_INSTALL_HEADERDIR_ABSPATH)
	mkdir -p $(ESMF_INSTALL_MODDIR_ABSPATH)
	cp -f $(ESMF_MODDIR)/*.mod $(ESMF_INSTALL_MODDIR_ABSPATH)
	mkdir -p $(ESMF_INSTALL_LIBDIR_ABSPATH)
	cp -f $(ESMF_LIBDIR)/* $(ESMF_INSTALL_LIBDIR_ABSPATH)
	mkdir -p $(ESMF_INSTALL_DOCDIR_ABSPATH)
	@if [ -d $(ESMF_DOCDIR) ]; then \
        cp -rf $(ESMF_DOCDIR)/* $(ESMF_INSTALL_DOCDIR_ABSPATH); \
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
	cd $(ESMF_DIR)/application ;\
	$(MAKE) clean
	cd $(ESMF_DIR)/application ;\
	$(MAKE)
	cd $(ESMF_DIR)/application ;\
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

