# $Id: makefile,v 1.6 2001/12/11 17:16:07 dneckels Exp $
#===============================================================================
#                            makefile
# 
# This is the primary makefile for building Modeling Framework (ESMF) utilities.  
#===============================================================================


ALL: all

DIRS = src
CLEANDIRS = lib mod test${BOPT}

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

build_libs:
	@${OMAKE} ESMF_DIR=${ESMF_DIR} ESMF_ARCH=${ESMF_ARCH} BOPT=${BOPT} ACTION=vpathlib tree 
	

#-------------------------------------------------------------------------------
# Basic targets to build ESMF libraries.
#-------------------------------------------------------------------------------
all       : info info_h chkalice_dir build_libs

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
	-@grep ESMC_VERSION_NUMBER include/ESMC_Version.h | ${SED} "s/........//"
	-@echo "-----------------------------------------"
	-@echo "Using ESMF flags: ${ALICEFLAGS} ${PCONF}"
	-@echo "-----------------------------------------"
	-@echo "Using configuration flags:"
	-@grep "define " build/${ESMF_ARCH}/conf.h
	-@echo "-----------------------------------------"
	-@echo "Using include paths: ${ESMC_INCLUDE}"
	-@echo "-----------------------------------------"
	-@echo "Using ESMF directory: ${ESMF_DIR}"
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
	-@echo  "Using ESMF flags: ${ALICEFLAGS} ${PCONF}" >> MINFO
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using configuration flags:" >> MINFO
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using include paths: ${ESMC_INCLUDE}" >> MINFO
	-@echo  "-----------------------------------------" >> MINFO
	-@echo  "Using ESMF directory: ${ESMF_DIR}" >> MINFO
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
	-@cd ${ESMF_DIR}/interface/F; \
	${OMAKE} BOPT=${BOPT} ESMF_DIR=${ESMF_DIR} ESMF_ARCH=${ESMF_ARCH} 
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
	-@mv ${ESMF_DIR}/interface/F90/*.mod ${ESMC_MODDIR}
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
	     build/*/conf.h build/*/fix.h build/win32/makefile.dos bin/config/base*.in \
             build/*/buildtest

DOCS	   = build/readme build/conf.defs

SCRIPTS    = maint/addlinks maint/builddist maint/buildlinks maint/wwwman \
	     maint/xclude maint/crontab  \
	     maint/autoftp include/foldinclude/generateincludes

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
	
tex: chkdir_doc
	-@echo "Building .F => .tex files"
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=buildtex  tree 

html: chkdir_doc tex
	-@echo "Building html files"
	-@echo "========================================="
	-@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=buildhtml  tree 

# Clean recursively deletes files that each makefile wants
# deleted.   Remove the .mod files here manually since the case
# of mods is not really predictable.
clean: chkopts
	@rm -f ${ESMC_MODDIR}/*.mod
	@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=clean_recursive  tree 

clobber: chkopts clean
	@${OMAKE} BOPT=${BOPT} ESMF_ARCH=${ESMF_ARCH} \
	   ACTION=clobber_recursive  tree 
