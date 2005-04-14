# $Id: makefile,v 1.60 2005/04/14 20:18:31 nscollins Exp $
#===============================================================================
#                            makefile
# 
# This is the primary makefile for building Earth System Modeling Framework
# (ESMF) utilities.  
#===============================================================================


#
# Build update 
# Aug 19, 2003
#

ifndef ESMF_DIR
$(error ESMF_DIR needs to be set to the top ESMF directory)
endif

# name of directory containing the ESMF source
ESMF_TOP_DIR   = $(ESMF_DIR)

# default target in common.mk is 'lib'

# all common rules, definitions are here
include $(ESMF_TOP_DIR)/build/common.mk

# things specific to framework build

DIRS = src

CLEANDIRS = $(ESMF_LIBDIR) $(ESMF_MODDIR) $(ESMF_TESTDIR) $(ESMF_EXDIR) \
	    $(ESMF_BUILD)/src/include
CLOBBERDIRS = $(ESMF_BUILD)/lib $(ESMF_BUILD)/mod \
	      $(ESMF_BUILD)/test $(ESMF_BUILD)/quick_start \
              $(ESMF_BUILD)/release $(ESMF_BUILD)/examples \
              $(ESMF_BUILD)/doc


#-------------------------------------------------------------------------------
# Basic targets to build ESMF libraries.
#-------------------------------------------------------------------------------
# Define what building "all" means for the framework.  This needs to be
# after the include of common.mk, so it does not interfere with the
# definition of the default build rule.

all:  lib build_unit_tests build_examples build_system_tests build_demos


#-------------------------------------------------------------------------------
# Prints information about the system and version of ESMF being compiled
#-------------------------------------------------------------------------------
script_info:
	-@echo " "
	-@echo "------------------------------------------"
	-@echo "Version information: "
	-@if [ -n "${C_CCV}" -a "${C_CCV}" != "unknown" ] ; then \
	  echo "C Compiler version:" ; ${C_CCV} ; echo "" ; fi
	-@if [ -n "${C_CXXV}" -a "${C_CXXV}" != "unknown" ] ; then \
	  echo "C++ Compiler version:" ; ${C_CXXV} ; echo "" ; fi
	-@if [ -n "${C_FCV}" -a "${C_FCV}" != "unknown" ] ; then \
	  echo "Fortran Compiler version:" ; ${C_FCV} ; echo "" ; fi
	-@if [ -f ${ESMF_DIR}/src/Infrastructure/Base/include/ESMC_Macros.h ] ; then \
	  fgrep ESMF_VERSION_STRING ${ESMF_DIR}/src/Infrastructure/Base/include/ESMC_Macros.h | ${SED} "s/^#define //" ; fi
	-@echo " "
	-@echo "------------------------------------------"
	-@echo "Using ESMF flags:"
	-@echo "ESMF_DIR: ${ESMF_TOP_DIR}"
	-@if [ "${ESMF_BUILD}" != "${ESMF_TOP_DIR}" ] ; then \
	  echo "ESMF_BUILD: ${ESMF_BUILD}" ; fi
	-@echo "ESMF_ARCH: ${ESMF_ARCH}"
	-@echo "ESMF_COMPILER: ${ESMF_COMPILER}"
	-@if [ -n "${ESMF_COMPILER_VERSION}" ] ; then \
	  echo "ESMF_COMPILER_VERSION: ${ESMF_COMPILER_VERSION}" ; fi
	-@if [ -n "${ESMF_C_COMPILER}" ] ; then \
	  echo "ESMF_C_COMPILER: ${ESMF_C_COMPILER}" ; fi
	-@echo "ESMF_BOPT: ${ESMF_BOPT}"
	-@echo "ESMF_PREC: ${ESMF_PREC}"
	-@echo "ESMF_COMM: ${ESMF_COMM}"
	-@echo "ESMF_SITE: ${ESMF_SITE}"
	-@echo "ESMF_EXHAUSTIVE: ${ESMF_EXHAUSTIVE}"
	-@echo "ESMF_PTHREADS: ${ESMF_PTHREADS}"
	-@if [ -n "${ESMF_NO_IOCODE}" ] ; then \
	  echo "ESMF_NO_IOCODE: ${ESMF_NO_IOCODE}" ; fi
#
info:   script_info
	-@echo " "
	-@echo "------------------------------------------"
	-@echo "Compilers, Flags, and Libraries:"
	-@echo "Location of the C compiler: " `which ${CC}`
	-@echo "Location of the C++ compiler: " `which ${CXX}`
	-@echo "Location of the Fortran compiler: " `which ${FC}`
	-@echo "Linking C with: ${CLINKER}"
	-@echo "Linking Fortran with: ${FLINKER}"
	-@echo ""
	-@echo "Compiling C with flags: ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS}"
	-@echo "Compiling C++ with flags: ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS}"
	-@echo "Linking C with flags: ${LINKOPTS}"
	-@echo "Linking C with libraries: -lesmf ${MPI_LIB} ${EXTRALIBS} ${CXXF90LIBS}"
	-@echo ""
	-@echo "Compiling Fortran with flags: ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS}"
	-@echo "Fortran module flag: ${FC_MOD}${ESMF_MODDIR}"
	-@echo "Linking Fortran with flags: ${LINKOPTS}"
	-@echo "Linking Fortran with libraries: -lesmf ${MPI_LIB} ${EXTRALIBS} ${F90CXXLIBS}"
	-@echo " "
	-@echo "------------------------------------------"
	-@echo Compiling on `date` on `hostname`
	-@echo Machine characteristics: `uname -a`
	-@echo "=========================================="
	-@echo " "
#
#
MINFO = ${ESMF_DIR}/build_config/${ESMF_ARCH}.$(ESMF_COMPILER).$(ESMF_SITE)/machineinfo.h
info_h:
	-@$(RM) MINFO ${MINFO}
	-@echo  "static char *machineinfo = \"  " >> MINFO
	-@$(MAKE) -s info >> MINFO 2>&1  
	-@cat MINFO | ${SED} -e 's/$$/  \\n\\/' > ${MINFO}
	-@echo  " \"; " >> ${MINFO}
	-@$(RM) MINFO

# Ranlib on the libraries
ranlib:
	${RANLIB} $(ESMF_LIBDIR)/*.a

# Deletes ESMF libraries
deletelibs: chkopts_basic
	-${RM} $(ESMF_LIBDIR)/*

# ESMF_COUPLED_FLOW/demo target.
ESMF_COUPLED_FLOW: chkopts build_libs chkdir_tests
	cd src/demo/coupled_flow ;\
	$(MAKE) BOPT=$(BOPT) demos

# ESMF_COUPLED_FLOW_uni/demo target.
ESMF_COUPLED_FLOW_uni: chkopts build_libs chkdir_tests
	cd src/demo/coupled_flow ;\
	$(MAKE) BOPT=$(BOPT) demos_uni


# ------------------------------------------------------------------
# All remaining actions are intended for ESMF developers only.
# ESMF users should not generally need to use these commands.


BUILDFILES = build/* build_config/*/*

DOCS	   = build/README build/doc/*

SCRIPTS    = 

# TODO: this target is not so useful right now - because we try
# to build shared libs on many of our platforms.  the executables
# linked against that lib (like the unit tests and examples) then
# store that build directory inside as where to find the shared
# lib at load time.  to move them, we have to know the eventual
# location of the libs (by having something like ESMF_INSTALL_DIR
# defined at the time the executables are linked, so we can add
# an extra -rpath or -L flag to the link to add the install lib
# dir to be searched at load time.
# also, this should just have a single variable and not separate
# ones for the lib, mod, and includes.
# also, there should be options to install just the bare lib,
# the unit tests and quickstart files, and then everything.
install:
	@if [ "${ESMF_LIB_INSTALL}" != "" ] ; then \
	 cp -fp $(ESMF_LIBDIR)/* ${ESMF_LIB_INSTALL} ; \
	fi
	if [ "${ESMF_MOD_INSTALL}" != "" ] ; then \
	 cp -fp ${ESMF_MODDIR}/*.mod ${ESMF_MOD_INSTALL} ; \
	fi
	if [ "${ESMF_H_INSTALL}" != "" ] ; then \
	 cp -fp $(ESMF_BUILD)/src/include/*.h ${ESMF_H_INSTALL} ; \
	fi


# ------------------------------------------------------------------
# add dummy rules here to avoid gnumake trying to remake the actual
# makefiles themselves; this might not be much of an overhead but 
# since we call make so many times recursively and it does the makefile
# remake rule check on each invocation of make, it effectively renders
# gmake -d (debug mode) almost unreadable.  this cuts the remake rule
# output down immensely.  nsc 05nov04

GNUmakefile:
	@echo ;

makefile:
	@echo ;

$(ESMF_TOP_DIR)/makefile:
	@echo ;

$(ESMF_TOP_DIR)/build/common.mk:
	@echo ;

$(ESMF_TOP_DIR)/build_config/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_SITE)/build_rules.mk:
	@echo ;

