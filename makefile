# $Id: makefile,v 1.48 2004/09/23 16:55:18 nscollins Exp $
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
all       : info info_h chk_dir build_libs shared

#-------------------------------------------------------------------------------
# Prints information about the system and version of ESMF being compiled
#-------------------------------------------------------------------------------
info:
	-@echo "=========================================="
	-@echo "=========================================="
	-@echo On `date` on `hostname`
	-@echo Machine characteristics: `uname -a`
	-@echo "-----------------------------------------"
	-@echo "Using C compiler: ${CC} ${COPTFLAGS} ${CCPPFLAGS}"
	-@if [ -n "${C_CCV}" -a "${C_CCV}" != "unknown" ] ; then \
	  echo "C Compiler version:" ; ${C_CCV} ; fi
	-@if [ -n "${CXX_CCV}" -a "${CXX_CCV}" != "unknown" ] ; then \
	  echo "C++ Compiler version:" ; ${CXX_CCV} ; fi
	-@echo "Using Fortran compiler: ${FC} ${FOPTFLAGS} ${FCPPFLAGS}"
	-@if [ -n "${C_FCV}" -a "${C_FCV}" != "unknown" ] ; then \
	  echo "Fortran Compiler version:" ; ${C_FCV} ; fi
	-@if [ -f ${ESMF_DIR}/src/include/ESMC_Macros.h ] ; then \
	  fgrep ESMF_VERSION_STRING ${ESMF_DIR}/src/include/ESMC_Macros.h | ${SED} "s/^#define //" ; fi
	-@echo "-----------------------------------------"
	-@echo "Using ESMF flags: ${PCONF}"
	-@echo "-----------------------------------------"
	-@echo "Using configuration flags:"
	-@grep "define " ${ESMF_TOP_DIR}/build_config/${ESMF_ARCH}.$(ESMF_COMPILER).$(ESMF_SITE)/ESMC_Conf.h
	-@echo "-----------------------------------------"
	-@echo "Using include paths: ${ESMC_INCLUDE}"
	-@echo "-----------------------------------------"
	-@echo "Using ESMF directory: ${ESMF_TOP_DIR}"
	-@echo "Using ESMF arch: ${ESMF_ARCH}"
	-@echo "------------------------------------------"
	-@echo "Using C linker: ${CLINKER}"
	-@echo "Using Fortran linker: ${FLINKER}"
	-@echo "Using libraries: ${ESMC_TIME_LIB}"
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
	cd src/demo/coupled_flow/src ;\
	$(MAKE) BOPT=$(BOPT) demo


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


