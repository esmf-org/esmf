# $Id: makefile,v 1.37 2003/11/10 22:35:35 nscollins Exp $
#===============================================================================
#                            makefile
# 
# This is the primary makefile for building Earth System Modeling Framework
# (ESMF) utilities.  
#===============================================================================


TOPALL: all

#
#  New Variables to point to build and top dirs.
#  DFF Feb 7, 2003
#ESMF_TOP_DIR   = $(ESMF_DIR)
#ESMF_BUILD_DIR = $(ESMF_DIR)/build

#Default build option.
#BOPT = g

#DIRS = src

#include $(ESMF_BUILD_DIR)/$(ESMF_ARCH)/base


#
# Build update 
# Aug 19, 2003
#

# name of directory containing the ESMF source
ESMF_TOP_DIR   = $(ESMF_DIR)
# name of directory containing the /build makefiles
ESMF_BUILD_DIR = $(ESMF_DIR)

include $(ESMF_BUILD_DIR)/build/common.mk

#
#  End Build Update 1
#

DIRS = src

CLEANDIRS = $(ESMF_LIBDIR) $(ESMF_MODDIR) $(ESMF_TESTDIR) doc 
CLOBBERDIRS = $(ESMF_BUILD)/lib $(ESMF_BUILD)/mod $(ESMF_BUILD)/test \
              $(ESMF_BUILD)/quick_start $(ESMF_BUILD)/release


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
	-@grep "define " ${ESMF_BUILD_DIR}/build_config/${ESMF_ARCH}.$(ESMF_COMPILER).$(ESMF_SITE)/ESMC_Conf.h
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

#-------------------------------------------------------------------------------
# Builds the Modeling Framework libraries and fortran77 and f90 interface
# files. (except compiling *.F files)
#-------------------------------------------------------------------------------

build_c:
	-@echo "BEGINNING TO COMPILE LIBRARIES IN ALL DIRECTORIES"
	-@echo "========================================="
	-@${OMAKE} ESMF_DIR=${ESMF_DIR} ESMF_ARCH=${ESMF_ARCH} BOPT=${BOPT} ACTION=libfast tree 
	-@cd ${ESMF_TOP_DIR}/interface/F; \
	${OMAKE} BOPT=${BOPT} ESMF_DIR=${ESMF_TOP_DIR} ESMF_ARCH=${ESMF_ARCH} 
	${RANLIB} $(ESMF_LIBDIR)/*.a
	-@echo "Completed building libraries"
	-@echo "========================================="

#------------------------------------------------------------------------------- 
# Builds the Modeling Framework Fortran source files
# Note:	 libfast cannot run on .F files on certain machines, so we
# use libf to compile the fortran source files.
#-------------------------------------------------------------------------------

build_fortran:
	-@echo "BEGINNING TO COMPILE FORTRAN SOURCE"
	-@echo "========================================="
	-@cd interface/F90; \
	  ${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} libf clean 
	-@mv ${ESMF_TOP_DIR}/interface/F90/*.mod ${ESMC_MODDIR}
	${RANLIB} $(ESMF_LIBDIR)/*.a
	-@echo "Completed compiling Fortran source"
	-@echo "========================================="

# Builds ESMF test examples for a given BOPT and architecture
test_cuni: info chkopts chkdir_tests
	-@echo "Beginning to compile and run Uniprocessor C test examples"
	-@echo "Due to different numerical round-off on certain"
	-@echo "machines some of the numbers may not match exactly."
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=testexamples_4  tree 
	-@echo "Completed compiling and running C test examples"
	-@echo "========================================="

# Builds ESMF test examples for a given BOPT and architecture
test_c: info chkopts chkdir_tests
	-@echo "Beginning to compile and run C test examples"
	-@echo "Due to different numerical round-off on certain"
	-@echo "machines some of the numbers may not match exactly."
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=testexamples_1  tree 
	-@echo "Completed compiling and running C test examples"
	-@echo "========================================="

# Builds ESMF test examples for a given BOPT and architecture
test_f90uni: info chkopts chkdir_tests
	-@echo "Beginning to compile and run Uniprocessor F90 test examples"
	-@echo "========================================="
	-@echo "Due to different numerical round-off on certain"
	-@echo "machines or the way Fortran formats numbers"
	-@echo "some of the results may not match exactly."
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=vtestexamples_9  tree 
	-@echo "Completed compiling and running F90 test examples"
	-@echo "========================================="

# Builds ESMF test examples for a given BOPT and architecture
test_f90: info chkopts chkdir_tests
	-@echo "Beginning to compile and run F90 test examples"
	-@echo "========================================="
	-@echo "Due to different numerical round-off on certain"
	-@echo "machines or the way Fortran formats numbers"
	-@echo "some of the results may not match exactly."
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=testexamples_3  tree 
	-@echo "Completed compiling and running F90 test examples"
	-@echo "========================================="

# Ranlib on the libraries
ranlib:
	${RANLIB} $(ESMF_LIBDIR)/*.a

# Deletes ESMF libraries
deletelibs: chkopts_basic
	-${RM} -f $(ESMF_LIBDIR)/*

# ESMF_COUPLED_FLOW/demo target.
ESMF_COUPLED_FLOW: chkopts build_libs chkdir_tests
	cd src/demo/coupled_flow ;\
	$(MAKE) BOPT=$(BOPT) demo


# ------------------------------------------------------------------
# All remaining actions are intended for ESMF developers only.
# ESMF users should not generally need to use these commands.


BUILDFILES = build/* build_config/*/*

DOCS	   = build/README build/doc/*

SCRIPTS    = 

install:
	-@if [ "${ESMF_LIB_INSTALL}" != "" ] ; then \
	cp $(ESMF_LIBDIR)/libesmf.a ${ESMF_LIB_INSTALL} ; \
	cp $(ESMF_LIBDIR)/libmpiuni.a ${ESMF_LIB_INSTALL} ; \
	fi
	-if [ "${ESMF_MOD_INSTALL}" != "" ] ; then \
	cp ${ESMC_MODDIR}/*.mod ${ESMF_MOD_INSTALL} ; \
	fi

# ------------------------------------------------------------------
# Rules for putting example files where they need to be for our
# binary releases (the pre-built libesmf.so and some simple examples).
# Creates the directory structure for releases, and copies example
# files and READMEs into it.
#
RELEASE_VERSION = 1_0_0rp2
RELEASE_SUBDIRS = example lib mod CoupledFlowSrc CoupledFlowExe
RELEASE_DIR     =  $(ESMF_BUILD)/release/esmf_$(RELEASE_VERSION)_$(ESMF_ARCH)_$(BOPT)_so

chkdir_release:
	@if [ ! -d $(RELEASE_DIR) ] ; then \
	   echo Making $(RELEASE_DIR); mkdir -p $(RELEASE_DIR) ; fi
	@for DIR in $(RELEASE_SUBDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      if [ ! -d $(RELEASE_DIR)/$$DIR ] ; then \
	         echo Making $(RELEASE_DIR)/$$DIR ;\
	         mkdir $(RELEASE_DIR)/$$DIR ;\
	      fi ;\
	   fi ;\
	done

build_release: chkdir_release build_libs shared
	$(MAKE) BOPT=$(BOPT) ACTION=tree_build_release tree

tree_build_release:
	@for FILES in $(RELEASE_COPYFILES) foo ; do \
	   if [ $$FILES != "foo" ] ; then \
	      echo "Copying $$FILES to $(RELEASE_DIR)/$(RELEASE_DESTDIR)" ;\
	      cp $$FILES $(RELEASE_DIR)/$(RELEASE_DESTDIR) ;\
	   fi ;\
	done
	@for FILES in $(RELEASE_ARCHCOPYFILES) foo ; do \
	   if [ $$FILES != "foo" ] ; then \
	      echo "Copying $$FILES.$(ESMF_ARCH) to $(RELEASE_DIR)/$(RELEASE_DESTDIR)/$$FILES" ;\
	      cp $$FILES.$(ESMF_ARCH) $(RELEASE_DIR)/$(RELEASE_DESTDIR)/$$FILES ;\
	   fi ;\
	done



# ------------------------------------------------------------------
# Rules for putting quick_start files where they need to be for our
# public releases. 
#

QUICKSTART_DIR     =  $(ESMF_BUILD)/quick_start

chkdir_quick_start:
	@if [ ! -d $(QUICKSTART_DIR) ] ; then \
	   echo Making $(QUICKSTART_DIR); mkdir -p $(QUICKSTART_DIR) ; fi
	@for DIR in $(QUICKSTART_SUBDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      if [ ! -d $(QUICKSTART_DIR)/$$DIR ] ; then \
	         echo Making $(QUICKSTART_DIR)/$$DIR ;\
	         mkdir $(QUICKSTART_DIR)/$$DIR ;\
	      fi ;\
	   fi ;\
	done

build_quick_start: chkdir_quick_start
	$(MAKE) BOPT=$(BOPT) ACTION=tree_build_quick_start tree

tree_build_quick_start:
	@for DIR in $(QUICKSTART_COPYDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      echo "Copying $$DIR files to $(QUICKSTART_DIR)" ;\
	      cp $$DIR/* $(QUICKSTART_DIR) ;\
	   fi ;\
	done






# Note: the following rules are currently in the build/common.mk 
#  makefile fragment.

# Clean recursively deletes files that each makefile wants
# deleted.   Remove the .mod files here manually since the case
# of mods is not really predictable.
# clean: 
# 	@rm -f ${ESMC_MODDIR}/*.mod
# 	@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
# 	   ACTION=clean_recursive  tree 

# clobber: chkopts clean
# 	@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
# 	   ACTION=clobber_recursive  tree 
