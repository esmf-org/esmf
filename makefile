# $Id: makefile,v 1.57 2005/02/03 16:45:09 jwolfe Exp $
#===============================================================================
#                            makefile
# 
# This is the primary makefile for building Earth System Modeling Framework
# (ESMF) utilities.  
#===============================================================================


TOPALL: all


#
# Build update 
# Aug 19, 2003
#

# name of directory containing the ESMF source
ESMF_TOP_DIR   = $(ESMF_DIR)


include $(ESMF_TOP_DIR)/build/common.mk

#
#  End Build Update 1
#

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
all       : info info_h chk_dir build_libs 

#-------------------------------------------------------------------------------
# Prints information about the system and version of ESMF being compiled
#-------------------------------------------------------------------------------
script_info:
	-@echo " "
	-@if [ -n "${C_CCV}" -a "${C_CCV}" != "unknown" ] ; then \
	  echo "C Compiler version:" ; ${C_CCV} ; echo "" ; fi
	-@if [ -n "${CXX_CCV}" -a "${CXX_CCV}" != "unknown" ] ; then \
	  echo "C++ Compiler version:" ; ${CXX_CCV} ; echo "" ; fi
	-@if [ -n "${C_FCV}" -a "${C_FCV}" != "unknown" ] ; then \
	  echo "Fortran Compiler version:" ; ${C_FCV} ; echo "" ; fi
	-@if [ -f ${ESMF_DIR}/src/Infrastructure/Base/include/ESMC_Macros.h ] ; then \
	  fgrep ESMF_VERSION_STRING ${ESMF_DIR}/src/Infrastructure/Base/include/ESMC_Macros.h | ${SED} "s/^#define //" ; fi
	-@echo "ESMF_DIR: ${ESMF_TOP_DIR}"
	-@echo "ESMF_ARCH: ${ESMF_ARCH}"
	-@echo "ESMF_COMPILER: ${ESMF_COMPILER}"
	-@echo "ESMF_PREC: ${ESMF_PREC}"
	-@echo "ESMF_SITE: ${ESMF_SITE}"
	-@echo "ESMF_COMM: ${ESMF_COMM}"
	-@echo "ESMF_BOPT: ${ESMF_BOPT}"
	-@if [ -n "${ESMF_COMPILER_VERSION}" ] ; then \
	  echo "ESMF_COMPILER_VERSION: ${ESMF_COMPILER_VERSION}" ; fi
	-@if [ -n "${ESMF_C_COMPILER}" ] ; then \
	  echo "ESMF_C_COMPILER: ${ESMF_C_COMPILER}" ; fi
#
info:   script_info
	-@echo "-----------------------------------------"
	-@echo "Using C compiler: ${CC} ${COPTFLAGS} ${CCPPFLAGS}"
	-@echo "Using Fortran compiler: ${FC} ${FOPTFLAGS} ${FCPPFLAGS}"
	-@echo "Using C linker: ${CLINKER}"
	-@echo "Using Fortran linker: ${FLINKER}"
	-@echo "------------------------------------------"
	-@echo "Using include paths: ${ESMC_INCLUDE}"
	-@echo "Using libraries: ${ESMC_TIME_LIB}"
	-@echo "------------------------------------------"
	-@echo Compiling on `date` on `hostname`
	-@echo Machine characteristics: `uname -a`
	-@echo "=========================================="
#
#
MINFO = ${ESMF_DIR}/build_config/${ESMF_ARCH}.$(ESMF_COMPILER).$(ESMF_SITE)/machineinfo.h
info_h:
	-@$(RM) -f MINFO ${MINFO}
	-@echo  "static char *machineinfo = \"  " >> MINFO
	-@echo  "Libraries compiled on `date` on `hostname` " >> MINFO
	-@echo  Machine characteristics: `uname -a` "" >> MINFO
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using C compiler: ${CC} ${COPTFLAGS} ${CCPPFLAGS} " >> MINFO
	-@if [  "${C_CCV}" -a "${C_CCV}" != "unknown" ] ; then \
	  echo  "C Compiler version:"  >> MINFO ; ${C_CCV} >> MINFO 2>&1; fi
	-@if [  "${CXX_CCV}" -a "${CXX_CCV}" != "unknown" ] ; then \
	  echo  "C++ Compiler version:"  >> MINFO; ${CXX_CCV} >> MINFO 2>&1 ; fi
	-@echo  "Using Fortran compiler: ${FC} ${FOPTFLAGS} ${FCPPFLAGS}" >> MINFO
	-@if [  "${C_FCV}" -a "${C_FCV}" != "unknown" ] ; then \
	  echo  "Fortran Compiler version:" >> MINFO ; ${C_FCV} >> MINFO 2>&1 ; fi
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using ESMF flags: ${PCONF}" >> MINFO
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using configuration flags:" >> MINFO
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using include paths: ${ESMC_INCLUDE}" >> MINFO
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using ESMF directory: ${ESMF_TOP_DIR}" >> MINFO
	-@echo  "Using ESMF arch: ${ESMF_ARCH}" >> MINFO
	-@echo  "------------------------------------------" >> MINFO
	-@echo  "Using C linker: ${CLINKER}" >> MINFO
	-@echo  "Using Fortran linker: ${FLINKER}" >> MINFO
	-@cat MINFO | ${SED} -e 's/$$/  \\n\\/' > ${MINFO}
	-@echo  "Using libraries: ${ESMC_TIME_LIB} \"; " >> ${MINFO}
	-@$(RM) MINFO

# Ranlib on the libraries
ranlib:
	${RANLIB} $(ESMF_LIBDIR)/*.a

# Deletes ESMF libraries
deletelibs: chkopts_basic
	-${RM} -f $(ESMF_LIBDIR)/*

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

install:
	-@if [ "${ESMF_LIB_INSTALL}" != "" ] ; then \
	cp -fp $(ESMF_LIBDIR)/libesmf.a ${ESMF_LIB_INSTALL} ; \
	cp -fp $(ESMF_LIBDIR)/libesmf.so ${ESMF_LIB_INSTALL} ; \
	cp -fp $(ESMF_LIBDIR)/libmpiuni.a ${ESMF_LIB_INSTALL} ; \
	fi
	-if [ "${ESMF_MOD_INSTALL}" != "" ] ; then \
	cp -fp ${ESMF_MODDIR}/*.mod ${ESMF_MOD_INSTALL} ; \
	fi
	-if [ "${ESMF_H_INSTALL}" != "" ] ; then \
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

