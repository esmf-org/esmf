# $Id: makefile,v 1.18 2003/03/20 20:32:31 flanigan Exp $
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
ESMF_TOP_DIR   = $(ESMF_DIR)
ESMF_BUILD_DIR = $(ESMF_DIR)/build


#Default build option.
BOPT = g

DIRS = src
CLEANDIRS = lib mod test

include $(ESMF_BUILD_DIR)/$(ESMF_ARCH)/base

build_libs:
	@${OMAKE} ESMF_DIR=${ESMF_DIR} ESMF_ARCH=${ESMF_ARCH} BOPT=${BOPT} ACTION=vpathlib tree 


#-------------------------------------------------------------------------------
# Basic targets to build ESMF libraries.
#-------------------------------------------------------------------------------
all       : info info_h chk_dir build_libs

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
	-@if [ -f ${ESMF_DIR}/src/include/ESMF_Version.inc ] ; then \
	  grep ESMF_VERSION_STRING ${ESMF_DIR}/src/include/ESMF_Version.inc | ${SED} "s/........//" ; fi
	-@echo "-----------------------------------------"
	-@echo "Using ESMF flags: ${PCONF}"
	-@echo "-----------------------------------------"
	-@echo "Using configuration flags:"
	-@grep "define " ${ESMF_BUILD_DIR}/${ESMF_ARCH}/conf.h
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
MINFO = ${ESMF_DIR}/build/${ESMF_ARCH}/machineinfo.h
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
	${RANLIB} ${PDIR}/*.a
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
	${RANLIB} ${PDIR}/*.a
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
	${RANLIB} ${PDIR}/*.a

# Deletes ESMF libraries
deletelibs: chkopts_basic
	-${RM} -f ${PDIR}/*

# ------------------------------------------------------------------
# All remaining actions are intended for ESMF developers only.
# ESMF users should not generally need to use these commands.


BUILDFILES = build/common* build/*/base build/*/base_variables build/*/base.site \
	     build/*/conf.h build/*/fix.h bin/config/base*.in \
             build/*/buildtest

DOCS	   = build/README build/conf.defs

SCRIPTS    = 

install:
	-@if [ "${ESMF_LIB_INSTALL}" != "" ] ; then \
	cp ${PDIR}/libesmf.a ${ESMF_LIB_INSTALL} ; \
	cp ${PDIR}/libmpiuni.a ${ESMF_LIB_INSTALL} ; \
	fi
	-if [ "${ESMF_MOD_INSTALL}" != "" ] ; then \
	cp ${ESMC_MODDIR}/*.mod ${ESMF_MOD_INSTALL} ; \
	fi


alldoc: chkdir_doc 
	-@echo "Building All Documentation"
	-@echo "========================================="
	-@${OMAKE} tex dvi pdf html


dvi: chkdir_doc tex
	-@echo "Building dvi files"
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=builddvi  tree 

pdf: chkdir_doc tex
	-@echo "Building dvi files"
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=buildpdf  tree 

# tex: chkdir_doc
# 	-@echo "Building .F => .tex files"
# 	-@echo "========================================="
# 	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
# 	   ACTION=buildtex  tree 

html: chkdir_doc tex
	-@echo "Building html files"
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=buildhtml  tree 


#
#  System Test Targets
#
system_tests: chkopts build_libs chkdir_tests
	@if [ -d src/SystemTests ] ; then cd src/SystemTests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
	   fi; \
        fi; \
	$(MAKE) BOPT=$(BOPT) ACTION=tree_system_tests tree

system_tests_uni: chkopts chkdir_tests
	@if [ -d src/SystemTests ] ; then cd src/SystemTests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
	   fi; \
        fi; \
	$(MAKE) BOPT=$(BOPT) ACTION=tree_system_tests_uni tree

build_system_tests: chkopts chkdir_tests
	@if [ -d src/SystemTests ] ; then cd src/SystemTests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
	   fi; \
        fi; \
	$(MAKE) BOPT=$(BOPT) ACTION=tree_build_system_tests tree

run_system_tests:  chkopts chkdir_tests
	@if [ -d src/SystemTests ] ; then cd src/SystemTests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
	   fi; \
        fi; \
	$(MAKE) BOPT=$(BOPT) ACTION=tree_run_system_tests tree

run_system_tests_uni:  chkopts chkdir_tests
	@if [ -d src/SystemTests ] ; then cd src/SystemTests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
	   fi; \
        fi; \
	$(MAKE) BOPT=$(BOPT) ACTION=tree_run_system_tests_uni tree


# Clean recursively deletes files that each makefile wants
# deleted.   Remove the .mod files here manually since the case
# of mods is not really predictable.
clean: 
	@rm -f ${ESMC_MODDIR}/*.mod
	@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=clean_recursive  tree 

clobber: chkopts clean
	@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=clobber_recursive  tree 
