#  $Id: common.mk,v 1.57 2004/06/29 14:12:30 nscollins Exp $
#===============================================================================
#   common.mk
#
#  GNU make makefile - cannot be used with standard unix make!!
#
#  This file contains variables and rules that are common across all
#  platforms.
#===============================================================================

#-------------------------------------------------------------------------------
#  If environment variables are not set give them default values.
#  For some variables having the literal string 'default' is ok; 
#  for others, look for this string and override it the same as 
#  if it was unset originally.
#-------------------------------------------------------------------------------

ifndef ESMF_ARCH
export ESMF_ARCH := $(shell uname -s)
endif
ifeq ($(ESMF_ARCH),default)
export ESMF_ARCH := $(shell uname -s)
endif

#-------------------------------------------------------------------------------
# Set defaults.
#
# Default value for ESMF_COMPILER is default 
# in most cases.  Except ...
# 
# When ESMF_ARCH is Darwin, then default value 
# for ESMF_COMPILER is absoft.
# 
# and...
#
# When ESMF_ARCH is Linux, then default value 
# for ESMF_COMPILER is lahey.
# -------------------------------------------------------------------------------

ifndef ESMF_COMPILER
export ESMF_COMPILER = default
endif

ifeq ($(ESMF_COMPILER),default)

ifeq ($(ESMF_ARCH),Darwin)
export ESMF_COMPILER = absoft
endif

ifeq ($(ESMF_ARCH),Linux)
export ESMF_COMPILER = lahey
endif

endif

ifndef ESMF_PREC
export ESMF_PREC = 64
endif
ifeq ($(ESMF_PREC),default)
export ESMF_PREC = 64
endif

# This is ok to remain as default
ifndef ESMF_SITE
export ESMF_SITE = default
endif

#
#  ESMF_COMM set in site files.
#

ifndef ESMF_BOPT
export ESMF_BOPT = O
endif
ifeq ($(ESMF_BOPT),default)
export ESMF_BOPT = O
endif



#-------------------------------------------------------------------------------
#  Common variables
#
# LIBNAME          - library name
# SOURCE           - source files
# SSOURCE          - single precision versions of some source code
# OBJS             - object files
# SOBJS            - single precision versions of some object files
# ESMC_INCLUDE     - locations of include files
# CPPFLAGS         - preprocessor flags for *.c, *.F preprocessing
# DOCS             - files that contain documentation, readmes etc.
# ESMC_PARCH       - corresponds to the PARCH_arch in the source files, set in 
#                    the file build/${ESMF_ARCH}/base
# ESMF_BUILD       - Root directory to build in.  Set this variable on the make
#                    line to build somewhere other than ESMF_DIR.
# ESMF_LIB_INSTALL - Directory for install target to place libs.
# ESMF_MOD_INSTALL - Directory for install target to place mod files.
#-------------------------------------------------------------------------------

# Comment out the following flag if you want to allow VM to use Pthreads
CPPFLAGS       += -DVM_DONT_SPAWN_PTHREADS

# Comment out the following lines if you want to include the IO code
CPPFLAGS       += -DESMF_NO_IOCODE
export ESMF_NO_IOCODE = true

ifndef ESMF_BUILD
export ESMF_BUILD := $(ESMF_TOP_DIR)
endif


LDIR		= $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)

ESMF_LIBDIR     = $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_MODDIR     = $(ESMF_BUILD)/mod/mod${ESMF_BOPT}/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_TESTDIR    = $(ESMF_BUILD)/test/test$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_EXDIR      = $(ESMF_BUILD)/examples/examples$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_INCDIR     = $(ESMF_BUILD)/src/include

# Building in the moddir solves problems about trying to copy module files
# in after the fact.
ESMC_OBJDIR	= ${ESMF_MODDIR}
ESMC_TESTDIR	= $(ESMF_BUILD)/test/test${ESMF_BOPT}/${ESMF_ARCH}.$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMC_DOCDIR	= $(ESMF_TOP_DIR)/doc
ESMF_BUILD_DOCDIR = $(ESMF_BUILD_DIR)/build/doc

PROTEX		= ${ESMF_TOP_DIR}/scripts/doc_templates/templates/protex 
CC_PROTEX       = ${ESMF_TOP_DIR}/scripts/doc_templates/templates/scripts/do_ccprotex 
CH_PROTEX       = ${ESMF_TOP_DIR}/scripts/doc_templates/templates/scripts/do_chprotex 
F_PROTEX        = ${ESMF_TOP_DIR}/scripts/doc_templates/templates/scripts/do_fprotex 

DO_LATEX	= ${ESMF_TOP_DIR}/scripts/doc_templates/templates/scripts/do_latex
DO_L2H		= ${ESMF_TOP_DIR}/scripts/doc_templates/templates/scripts/do_l2h

LIBNAME		= $(ESMF_LIBDIR)/${LIBBASE}.a
ESMFLIB		= $(ESMF_LIBDIR)/libesmf.a

SOURCE		= ${SOURCEC} ${SOURCEF}
OBJS		= ${OBJSC} ${OBJSF}

DO_UT_RESULTS	= ${ESMF_TOP_DIR}/scripts/test_scripts/do_ut_results
DO_EX_RESULTS	= ${ESMF_TOP_DIR}/scripts/test_scripts/do_ex_results
DO_ST_RESULTS	= ${ESMF_TOP_DIR}/scripts/test_scripts/do_st_results
DO_SUM_RESULTS	= ${ESMF_TOP_DIR}/scripts/test_scripts/do_summary

ESMC_INCLUDE	= -I${ESMF_TOP_DIR}/${LOCDIR} \
		  -I${ESMF_TOP_DIR}/${LOCDIR}/../include \
		  ${LOCAL_INCLUDE} \
		  -I${ESMF_BUILD_DIR}/build_config/${ESMF_ARCH}.$(ESMF_COMPILER).$(ESMF_SITE) \
		  -I$(ESMF_INCDIR) -I$(ESMF_MODDIR) $(MPI_INCLUDE) \
                  ${NETCDF_INCLUDE}

CCPPFLAGS	+= ${PCONF} ${ESMC_PARCH} -DS${ESMF_PREC}=1 ${CPPFLAGS} \
	 	  -D__SDIR__='"${LOCDIR}"'
FCPPFLAGS	+= ${PCONF} ${ESMC_PARCH} -DS${ESMF_PREC}=1 ${CPPFLAGS} \
                   $(FCPP_EXHAUSTIVE)
C_SH_LIB_PATH	= ${CLINKER_SLFLAG}${LDIR} ${C_DYLIBPATH}
F_SH_LIB_PATH	= ${FLINKER_SLFLAG}${LDIR} ${F_DYLIBPATH}

ESMC_TIME_LIB	 = -L${LDIR}

#-------------------------------------------------------------------------------
# Defines all libraries needed for using linear and nonlinear solvers.
# The order of listing these libraries is important!
#
# PCONF - indicates which OPTIONAL external packages are available at your site
#-------------------------------------------------------------------------------

PCONF		= ${ESMC_HAVE_MPE}  ${ESMC_HAVE_PARMETIS} \
                  ${ESMC_HAVE_AMS}  ${ESMC_HAVE_X11}   ${ESMC_HAVE_MATLAB} \
                  ${ESMC_HAVE_ADIC} ${ESMC_HAVE_JAVA}
EXTERNAL_LIB	= ${MPE_LIB}        ${BLOCKSOLVE_LIB}  ${PARMETIS_LIB} \
                  ${AMS_LIB}        ${SPAI_LIB} \
                  ${ADIC_LIB} 


#-------------------------------------------------------------------------------
# ESMF_EXHAUSTIVE is passed (by CPP) into test programs to control the number 
# of tests that a test program will do.
#-------------------------------------------------------------------------------

ifeq ($(ESMF_EXHAUSTIVE),ON) 
FCPP_EXHAUSTIVE  = $(FPP_PREFIX)-DESMF_EXHAUSTIVE 
CCPPFLAGS       += -DESMF_EXHAUSTIVE 
endif

#-------------------------------------------------------------------------------
#  Common variable definitions.
#-------------------------------------------------------------------------------

CC	       = ${C_CC}
CXX	       = ${CXX_CC}
FC	       = ${C_FC}
CPP	       = gcc
M4	       = m4
CLINKER_SLFLAG = ${C_CLINKER_SLFLAG}
FLINKER_SLFLAG = ${C_FLINKER_SLFLAG}
CLINKER	       = ${C_CLINKER} ${COPTFLAGS} ${C_SH_LIB_PATH}

# C++ <=> F90 
F90CXXLIBS     = ${C_F90CXXLIBS}
CXXF90LIBS     = ${C_CXXF90LIBS}
CXXSO          = ${C_CXXSO}
CXXSOLIBS      = ${C_CXXSOLIBS}
ESMC_LANGUAGE = CONLY
ESMC_SCALAR   = real
SYS_LIB	       = ${C_SYS_LIB}

#-------------------------------------------------------------------------------
# Variable definitions for debug option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),g)

CXXF90LD       = ${C_CXXF90LD} -L$(ESMF_LIBDIR)
F90CXXLD       = ${C_F90CXXLD} -L$(ESMF_LIBDIR)
FLINKER	       = ${C_FLINKER} ${FOPTFLAGS} ${F_SH_LIB_PATH}
COPTFLAGS      = ${G_COPTFLAGS}
FOPTFLAGS      = ${G_FOPTFLAGS}
BBOPT	       = ${G_BBOPT}

endif

#-------------------------------------------------------------------------------
# Variable definitions for optimize option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),O)


CXXF90LD       = ${C_CXXF90LD} -L$(ESMF_LIBDIR)
F90CXXLD       = ${C_F90CXXLD} -L$(ESMF_LIBDIR)
FLINKER	       = ${C_FLINKER} ${FOPTFLAGS} ${F_SH_LIB_PATH}
COPTFLAGS      = ${O_COPTFLAGS}
FOPTFLAGS      = ${O_FOPTFLAGS}
BBOPT	       = ${O_BBOPT}

endif

#-------------------------------------------------------------------------------
# Checks that ESMF_DIR variable is set and creates library directory
# if it does not exist
#-------------------------------------------------------------------------------
chk_dir:
	@if [ ${ESMF_BOPT}foo = foo ] ; then \
	  echo "You must use the make variable ESMF_BOPT=[g,O,Opg,O_c++,O_complex,...]" ; \
	  echo "For example, use: make ESMF_BOPT=g ex1"; \
          echo "Remove all .o files and rerun make with appropriate ESMF_BOPT"; false; fi
	-@if [ ! -d $(ESMF_LIBDIR) ]; then \
	  echo Making directory $(ESMF_LIBDIR) for library; mkdir -p $(ESMF_LIBDIR) ; fi
	-@if [ ! -d ${ESMF_MODDIR} ]; then \
	  echo Making directory ${ESMF_MODDIR} for *.mod files; mkdir -p ${ESMF_MODDIR} ; fi

chkdir_doc:
	-@if [ ! -d ${ESMC_DOCDIR} ]; then \
	  echo Making directory ${ESMC_DOCDIR} for documents; mkdir -p ${ESMC_DOCDIR} ; fi

chkdir_tests:
	-@if [ ! -d ${ESMC_TESTDIR} ]; then \
	  echo Making directory ${ESMC_TESTDIR} for test output; mkdir -p ${ESMC_TESTDIR} ; fi

chkdir_include:
	-@if [ ! -d $(ESMF_INCDIR) ]; then \
	  echo Making directory $(ESMF_INCDIR) for test output; mkdir -p $(ESMF_INCDIR) ; fi

chkdir_examples:
	-@if [ ! -d ${ESMF_EXDIR} ]; then \
	  echo Making directory ${ESMF_EXDIR} for examples output; mkdir -p ${ESMF_EXDIR} ; fi


#-------------------------------------------------------------------------------
# 1. Checks that user has set ESMF_BOPT variable
# 2. Check if the ${LDIR} exists
#-------------------------------------------------------------------------------
chkopts:
	@if [ ${ESMF_BOPT}foo = foo ] ; then \
	  echo "You must set the variable ESMF_BOPT=[g,O,Opg,O_c++,O_complex,...]" ; \
	  echo "For example, use: make ESMF_BOPT=g ex1"; \
          echo "Remove all .o files and rerun make with appropriate ESMF_BOPT"; false; fi

# Does nothing; needed for some rules that require actions.
foo:

VPATH = ${ESMF_TOP_DIR}/${LOCDIR}:${ESMF_TOP_DIR}/include

libc:${LIBNAME}(${OBJSC})
libf:${LIBNAME}(${OBJSF})

storeh: chkdir_include
	for hfile in ${STOREH} foo ; do \
	  if [ $$hfile != "foo" ]; then \
	    cp -f ${ESMF_TOP_DIR}/${LOCDIR}/../include/$$hfile $(ESMF_INCDIR) ; \
	  fi ; \
	done


#-------------------------------------------------------------------------------
# Builds ESMF recursively.
#-------------------------------------------------------------------------------
build_libs: chk_dir
	cd $(ESMF_TOP_DIR) ;\
	${OMAKE} ESMF_DIR=${ESMF_DIR} ESMF_ARCH=${ESMF_ARCH} ESMF_BOPT=${ESMF_BOPT} ACTION=vpathlib tree shared

# Build only stuff below the current dir.
build_here: chk_dir
	${OMAKE} ESMF_DIR=${ESMF_DIR} ESMF_ARCH=${ESMF_ARCH} ESMF_BOPT=${ESMF_BOPT} ACTION=vpathlib tree shared

# Builds library
vpathlib:
	dir=`pwd`; cd ${ESMC_OBJDIR}; ${OMAKE} -f $${dir}/makefile MAKEFILE=$${dir}/makefile lib

# Builds library
lib:: chk_dir ${SOURCE}
	@if [ "${STOREH}" != "" ] ; then \
	   $(MAKE) -f ${MAKEFILE} ESMF_ARCH=${ESMF_ARCH} ESMF_BOPT=${ESMF_BOPT} storeh; fi
	@if [ "${SOURCEC}" != "" ] ; then \
	   $(MAKE) -f ${MAKEFILE} ESMF_ARCH=${ESMF_ARCH} ESMF_BOPT=${ESMF_BOPT} libc; fi
	@if [ "${SOURCEF}" != "" ] ; then \
	    $(MAKE) -f ${MAKEFILE}  ESMF_ARCH=${ESMF_ARCH} ESMF_BOPT=${ESMF_BOPT} libf; fi
	@if [ "${OBJS}" != " " ] ; then \
		${RANLIB} ${LIBNAME}; \
		${RM} -f ${OBJS}; \
	fi
	@if [ "${QUICKSTART}" != "" ] ; then \
	   $(MAKE) -f ${MAKEFILE} ESMF_ARCH=${ESMF_ARCH} ESMF_BOPT=${ESMF_BOPT} tree_build_quick_start; fi

#
#  Does not work for some machines with .F fortran files.
#
# Builds library - fast version
libfast: chk_dir ${SOURCEC} ${SOURCEF}
	@-if [ "${SOURCEC}" != "" ] ; then \
	     ${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${SOURCEC} ${SSOURCE} ;\
	  ${AR} ${AR_FLAGS} ${LIBNAME} ${OBJSC} ${SOBJS}; \
	  ${RM} -f ${OBJSC} ${SOBJS}; \
	fi

#-------------------------------------------------------------------------------
# Clean and clobber targets.
#  
# The clean and clobber targets are controlled by the settings of the
# variables CLEANFILES, CLEANDIRS and CLOBBERDIRS.  These variables
# are set in the local makefiles.
# 
#    CLEANFILES lists the files that should be removed during a clean.
# 
#    CLEANDIRS lists the directories that should be removed during a
#    clean.
#
#    CLOBBERDIRS lists the directories that should be removed during a
#    clobber.
#
# The clean and clobber targets recursively call make with the tree
# target.  The current directory and directories below will be cleaned
# or clobbered.  The clobber target first calls gmake with the clean target
# before the clobber actions are taken.
# -------------------------------------------------------------------------------

clean:
	$(MAKE) ACTION=tree_clean tree


clobber: clean
	@for DIR in $(CLOBBERDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      rm -rf $$DIR ;\
	   fi ;\
	done


tree_clean:
	@for DIR in $(CLEANDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      echo rm -rf $$DIR ;\
	      rm -rf $$DIR ;\
	   fi ;\
	done
	rm -f $(CLEANFILES)

#-------------------------------------------------------------------------------
# Targets for building and running system tests.
#-------------------------------------------------------------------------------

system_tests: chkopts build_libs chkdir_tests
	@if [ -d src/system_tests ] ; then cd src/system_tests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
               echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
               exit; \
	   fi; \
        fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_system_tests tree
	$(DO_ST_RESULTS)

tree_system_tests: tree_build_system_tests tree_run_system_tests

#
# system_tests_uni, build and run uni versions of the system tests
#
system_tests_uni: chkopts chkdir_tests
	@if [ -d src/system_tests ] ; then cd src/system_tests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
        fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_system_tests_uni tree
	$(DO_ST_RESULTS)


tree_system_tests_uni: tree_build_system_tests tree_run_system_tests_uni

#
# build_system_tests
#
build_system_tests: chkopts chkdir_tests
	@if [ -d src/system_tests ] ; then cd src/system_tests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
        fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_build_system_tests tree

tree_build_system_tests:  $(SYSTEM_TESTS_BUILD) 

#
#  Link rule for Fortran system tests.
#
$(ESMC_TESTDIR)/ESMF_%STest : ESMF_%STest.o $(SYSTEM_TESTS_OBJ) $(ESMFLIB)
	-$(SL_F_LINKER) -o $@ $(SYSTEM_TESTS_OBJ) $< -lesmf \
	${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} \
	$(SL_LINKOPTS)
	${RM} -f *.o *.mod

#
# run_system_tests
#
run_system_tests:  chkopts chkdir_tests
	@if [ -d src/system_tests ] ; then cd src/system_tests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
        fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_system_tests tree
	$(DO_ST_RESULTS)

tree_run_system_tests: $(SYSTEM_TESTS_RUN) 

#
# run_system_tests_uni
#
run_system_tests_uni:  chkopts chkdir_tests
	@if [ -d src/system_tests ] ; then cd src/system_tests; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
        fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_system_tests_uni tree
	$(DO_ST_RESULTS)

tree_run_system_tests_uni: $(SYSTEM_TESTS_RUN_UNI)

#
# report statistics on system tests
#
check_system_tests:
	$(DO_ST_RESULTS)


#-------------------------------------------------------------------------------
#  Targets for building and running unit tests.
#-------------------------------------------------------------------------------

tests: chkopts chkdir_tests build_libs
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_tests tree
	$(DO_UT_RESULTS)

tree_tests: tree_build_tests tree_run_tests

#
# tests_uni
#
tests_uni: chkopts chkdir_tests
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_tests_uni tree
	$(DO_UT_RESULTS)

tree_tests_uni: tree_build_tests tree_run_tests_uni

#
# build_tests
#
build_tests: chkopts chkdir_tests
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_build_tests tree

tree_build_tests: $(TESTS_BUILD) 

$(ESMC_TESTDIR)/ESMF_%UTest : ESMF_%UTest.o $(ESMFLIB)
	-$(SL_F_LINKER) -o $@  $(UTEST_$(*)_OBJS) $< -lesmf \
	${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} \
	$(SL_LINKOPTS)
	${RM} -f *.o *.mod


#
# run_tests
#
run_tests:  chkopts chkdir_tests
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_tests tree
	$(DO_UT_RESULTS)

tree_run_tests: $(TESTS_RUN) 

#
# run_tests_uni
#
run_tests_uni:  chkopts chkdir_tests
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_tests_uni tree 
	$(DO_UT_RESULTS)

tree_run_tests_uni: $(TESTS_RUN_UNI)

#
# report statistics on tests
#
check_tests:
	$(DO_UT_RESULTS)

#-------------------------------------------------------------------------------
# Targets for building and running examples
#
#  The example targets depend on variables set in the
#  makefile in the example source code directory.  The 
#  variables are:
#
#      EXAMPLES_BUILD should list the full path all of the 
#      example executables to be made.  The example executables
#      have to be built in $(ESMF_EXDIR).  
#
#      EXAMPLES_RUN and EXAMPLES_RUN_UNI list the run targets
#      of the individual executables.  The run targets are defined
#      in the source code makefiles.  EXAMPLES_RUN list the targets
#      that run the examples on multiply processors.  EXAMPLES_RUN_UNI
#      lists the targets that run the examples on single processors.
#
#-------------------------------------------------------------------------------      

#
# examples
#
examples: chkopts chkdir_examples build_libs
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_examples tree
	$(DO_EX_RESULTS)


tree_examples: tree_build_examples tree_run_examples

#
# examples_uni
#
examples_uni: chkopts chkdir_examples  
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_examples_uni tree

tree_examples_uni: tree_build_examples tree_run_examples_uni

#
# build_examples
#
build_examples: chkopts chkdir_examples
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_build_examples tree

tree_build_examples: $(EXAMPLES_BUILD) 

#
#  Examples Link commands
#
$(ESMF_EXDIR)/ESMF_%Ex : ESMF_%Ex.o $(ESMFLIB)
	-$(SL_F_LINKER) -o $@ $< -lesmf ${F90CXXLIBS} \
	${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} \
	$(SL_LINKOPTS)
	rm -f  $<


$(ESMF_EXDIR)/ESMC_%Ex: ESMC_%Ex.o $(ESMFLIB)
	-${SL_C_LINKER} -g -o $@ $< -lesmf \
        ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} \
        $(SL_LINKOPTS)
	rm -f $<

#
# run_examples
#
run_examples:  chkopts chkdir_examples
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_examples tree
	$(DO_EX_RESULTS)

tree_run_examples: $(EXAMPLES_RUN) 

#
# run_examples_uni
#
run_examples_uni:  chkopts chkdir_examples
	-$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_examples_uni tree 
	$(DO_EX_RESULTS)

tree_run_examples_uni: $(EXAMPLES_RUN_UNI)

#
# report statistics on examples
#
check_examples:
	$(DO_EX_RESULTS)


#-------------------------------------------------------------------------------
# Targets for building and running demos.
#-------------------------------------------------------------------------------

demo: chkopts build_libs chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_demo tree

tree_demo: tree_build_demo tree_run_demo

demo_uni: chkopts build_libs chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_demo_uni tree

tree_demo_uni: tree_build_demo tree_run_demo_uni

#
# build_demo
#
build_demo: chkopts chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_build_demo tree

tree_build_demo: $(DEMO_BUILD) 

$(ESMC_TESTDIR)/%App : %Demo.o $(DEMO_OBJ) $(ESMFLIB)
	$(SL_F_LINKER) -o $@ $(DEMO_OBJ) $< -lesmf ${F90CXXLIBS} \
	${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} \
	$(SL_LINKOPTS)
	${RM} -f *.o *.mod


#
# run_demo
#
run_demo:  chkopts chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_demo tree

tree_run_demo: $(DEMO_RUN) 

run_demo_uni:  chkopts chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ESMF_BOPT=$(ESMF_BOPT) ACTION=tree_run_demo_uni tree

tree_run_demo_uni: $(DEMO_RUN_UNI) 


#-------------------------------------------------------------------------------
# Targets for checking the builds
#-------------------------------------------------------------------------------

check_results: check_tests check_examples check_system_tests

results_summary:
	@$(DO_SUM_RESULTS)


#-------------------------------------------------------------------------------
# Targets for building the release, and the quick_start dirs
#-------------------------------------------------------------------------------

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

tree_build_quick_start: chkdir_quick_start
	@for DIR in $(QUICKSTART_COPYDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      echo "Copying $$DIR files to $(QUICKSTART_DIR)" ;\
	      cp -f $$DIR/* $(QUICKSTART_DIR) ;\
	   fi ;\
	done





#-------------------------------------------------------------------------------
#  Doc targets
#-------------------------------------------------------------------------------

doc: chkdir_doc tex
	-@echo "========================================="
	cd $(ESMF_TOP_DIR)/src/doc ;\
	$(MAKE) $(DVIFILES) $(PDFFILES) $(HTMLFILES) 
	-@echo "Build doc completed."

alldoc: chkdir_doc 
	-@echo "Building All Documentation"
	-@echo "========================================="
	-@$(MAKE) tex dvi pdf html
	-@echo "Build alldoc completed."

tex:
	-@echo "Building .tex files"
	-@echo "========================================="
	$(MAKE) ACTION=tree_tex tree

tree_tex: $(TEXFILES_TO_MAKE)

dvi: chkdir_doc tex
	-@echo "Building .dvi files"
	-@echo "========================================="
	-@${OMAKE} ACTION=tree_dvi  tree       

tree_dvi: chkdir_doc ${DVIFILES}


pdf: chkdir_doc
	-@echo "Building .pdf files"
	-@echo "========================================="
	-@${OMAKE} ACTION=tree_pdf  tree 

tree_pdf: chkdir_doc ${PDFFILES}


html: chkdir_doc
	-@echo "Building .html files"
	-@echo "========================================="
	-@${OMAKE} ACTION=tree_html tree 

tree_html:chkdir_doc ${HTMLFILES}


#-------------------------------------------------------------------------------
# Recursive calls
#-------------------------------------------------------------------------------

tree: $(ACTION)
	@if [ "${DIRS}" != "" ]; then \
	  for dir in ${DIRS} foo ; do \
            if [ -d $$dir ]; then \
              (cd $$dir ; \
              echo $(ACTION) in: `pwd`; \
              $(MAKE) -f makefile tree ACTION=$(ACTION));\
              if [ "$$?" != 0 ]; then \
                exit 1; \
              fi; \
            fi; \
	  done; \
        fi


#-------------------------------------------------------------------------------
# Suffixes
#-------------------------------------------------------------------------------
.SUFFIXES: .f .f90 .F .F90 ${SUFFIXES} .C .cc .cpp .r .rm .so

#-------------------------------------------------------------------------------
#  Compile rules for F90, C++, and c files for both to .o and .a files
#-------------------------------------------------------------------------------

.F90.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREENOCPP} ${ESMC_INCLUDE} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXCPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXNOCPP} ${ESMC_INCLUDE} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREENOCPP} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${FCPPFLAGS} ${F_FIXCPP} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXNOCPP} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.c.a:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.C.a:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

# The rules below generate a valid Fortran file using gcc as a cpp.
# The -P option prevents putting #line directives in the output, and
# -E stops after preprocessing.  The 'tr' command substitutes one-for-one,
# translating @s into newlines to separate multiline macros, and
# also translate ^ into # so that other include files are ready to
# be processed by the second runthru of the preprocessor during the
# actual compile. (These lines are: ^include "fred.h" in the
# original source to shield them from the first preprocess pass.)
# the dir, notdir macros below are to be sure to create the .F90 file
# in the original source directory, since the makefile has already
# changed dirs into the mod dir to build.  The sed command removes
# any lines which start #pragma GCC ...  these are generated by some
# versions of gcc and confuse other versions of the fortran preprocessor.
#
.cpp.F90:
	${CPP} -E -P -I${ESMF_INCDIR} $< | tr "@^" "\n#" | sed -e '/^#pragma GCC/d' > $(dir $<)$(notdir $@)


.cpp.o:
	${CPP} -E -P -I${ESMF_INCDIR} $< | tr "@^" "\n#" | sed -e '/^#pragma GCC/d' > $(dir $<)$(basename $@).F90
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $(dir $<)$(basename $@).F90


#-------------------------------------------------------------------------------
#  Build shared library from regular lib (.so from .a)
#-------------------------------------------------------------------------------
build_shared:
	@echo making shared libraries in $(LDIR) 
	@cd $(LDIR) ;\
	rm -rf tmp* *.so;\
	for NEXTLIB in $(SL_LIBS_TO_MAKE) foo ;\
	do \
	if [ -f $$NEXTLIB.a ] ; then \
	    echo Converting $$NEXTLIB.a to $$NEXTLIB.$(SL_SUFFIX) ;\
	    mkdir tmp_$$NEXTLIB ;\
	    cd tmp_$$NEXTLIB  ;\
            $(AR) $(AR_EXTRACT) ../$$NEXTLIB.a ;\
	    echo $(SL_LIB_LINKER) $(SL_LIBOPTS) -o $(LDIR)/$$NEXTLIB.$(SL_SUFFIX) *.o ;\
	    $(SL_LIB_LINKER) $(SL_LIBOPTS) -o $(LDIR)/$$NEXTLIB.$(SL_SUFFIX) *.o ;\
	    cd .. ;\
	    rm -rf tmp_$$NEXTLIB ;\
	fi ;\
	done 

#-------------------------------------------------------------------------------
# Pattern rules for making Tex files using protex script.  Input to 
# protex script is Fortran, C or .h source code.
#-------------------------------------------------------------------------------

%_fapi.tex : ../src/%.F 
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../src/%.F90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../src/%.f
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../src/%.f90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../interface/%.F 
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../interface/%.F90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../interface/%.f
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../interface/%.f90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_chapi.tex : ../include/%.h
	export PROTEX=$(PROTEX) ;\
	$(CH_PROTEX) $* $<

%_ccapi.tex : ../src/%.C
	export PROTEX=$(PROTEX) ;\
	$(CC_PROTEX) $* $<

%_ccapi.tex : ../interface/%.C
	export PROTEX=$(PROTEX) ;\
	$(CC_PROTEX) $* $<

%_fapi.tex : ../examples/%.F 
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../examples/%.F90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../examples/%.f
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_fapi.tex : ../examples/%.f90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $<

%_ccapi.tex : ../examples/%.C
	export PROTEX=$(PROTEX) ;\
	$(CC_PROTEX) $* $<

# special for the AppDriver dir. 
%SeqPrF_fapi.tex : ../seq_pairwise_fdriver_spmd/%.F90
	export PROTEX=$(PROTEX) ;\
	$(F_PROTEX) $* $< SeqPrF



#-------------------------------------------------------------------------------
# Pattern rules for making dvi, pdf and html files.
#-------------------------------------------------------------------------------

# The variable TEXINPUTS_VALUE can be set in the makefiles of the
# source code tree to list the directory paths of the .tex and
# graphics files included by .tex sources files.  The paths listed in
# TEXINPUTS_VALUE should be colon separated.  If TEXINPUTS_VALUE is
# set, then the first path should a period for the local directory.  A
# colon should trail the last path listed. If TEXINPUTS_VALUE is not
# set, then only files in the local directory will be found by the
# \input and \includegraphics LaTeX commands.
#

TEXINPUTS_VALUE = ".:$(ESMF_DIR)/src/doc:$(ESMF_BUILD_DOCDIR):$(ESMF_DIR)/src/demo/coupled_flow:" 

#-------------------------------------------------------------------------------
#  dvi rules
#-------------------------------------------------------------------------------
%_desdoc.dvi : %_desdoc.ctex $(DESDOC_DEP_FILES)
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* des

%_refdoc.dvi : %_refdoc.ctex $(REFDOC_DEP_FILES)
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* ref

%_reqdoc.dvi : %_reqdoc.ctex $(REQDOC_DEP_FILES)
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* req

#-------------------------------------------------------------------------------
#  pdf rules
#-------------------------------------------------------------------------------

$(ESMC_DOCDIR)/%.pdf: %.dvi
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	dvipdf $< $@

#-------------------------------------------------------------------------------
#  html rules
#-------------------------------------------------------------------------------
$(ESMC_DOCDIR)/%_desdoc: %_desdoc.ctex $(DESDOC_DEP_FILES)
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} $* des
	rm -f .latex2html-init
	mv -f $(@F) $(ESMC_DOCDIR)


$(ESMC_DOCDIR)/%_refdoc: %_refdoc.ctex $(REFDOC_DEP_FILES)
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} $* ref
	rm -f .latex2html-init
	mv -f $(@F) $(ESMC_DOCDIR)

$(ESMC_DOCDIR)/%_reqdoc: %_reqdoc.ctex $(REQDOC_DEP_FILES)
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} $* req
	rm -f .latex2html-init
	mv -f $(@F) $(ESMC_DOCDIR)

#-------------------------------------------------------------------------------
#  These rules are for compiling the test examples.
#-------------------------------------------------------------------------------
.cpp.rm .cc.rm .C.rm .F.rm .f.rm .c.rm:
	-@${RM} -f $* *.o $*.mon.* gmon.out mon.out


#-------------------------------------------------------------------------------
# Keep .o files
#-------------------------------------------------------------------------------
.PRECIOUS: %.o


#-------------------------------------------------------------------------------
#  Include site specific makefile fragment.
#-------------------------------------------------------------------------------
include $(ESMF_BUILD_DIR)/build_config/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_SITE)/build_rules.mk

#  DO NOT REMOVE These examples are used by the implementation report.
#
#  testexamples_X - Runs various test suites
#    1 - basic C suite used in installation tests
#    2 - additional C suite including graphics
#    3 - basic Fortran .F suite
#    4 - uniprocessor version of 1 and 2
#    5 - C examples that require complex numbers
#    6 - C examples that don't work with complex numbers
#    7 - C examples that require BlockSolve
#    8 - Fortran .F examples that don't work with complex numbers
#    9 - uniprocessor version of 3
#   10 - Fortran examples that require complex
#   11 - uniprocessor version of 5
#   12 - basic f90 examples
#   13 - Examples that should only be compiled.
#
testexamples_1: ${TESTEXAMPLES_1}
vtestexamples_1:
        dir=`pwd`; cd ${ESMC_TESTDIR}; ${OMAKE} -f $${dir}/makefile MAKEFILE=$${dir}/makefile testexamples_1
testexamples_2: ${TESTEXAMPLES_2}
testexamples_3: ${TESTEXAMPLES_3}
vtestexamples_3:
        dir=`pwd`; cd ${ESMC_TESTDIR}; ${OMAKE} -f $${dir}/makefile MAKEFILE=$${dir}/makefile testexamples_3
testexamples_4: ${TESTEXAMPLES_4}
vtestexamples_4:
        dir=`pwd`; cd ${ESMC_TESTDIR}; ${OMAKE} -f $${dir}/makefile MAKEFILE=$${dir}/makefile testexamples_4
testexamples_5: ${TESTEXAMPLES_5}
testexamples_6: ${TESTEXAMPLES_6}
testexamples_7: ${TESTEXAMPLES_7}
testexamples_8: ${TESTEXAMPLES_8}
testexamples_9: ${TESTEXAMPLES_9}
vtestexamples_9:
        dir=`pwd`; cd ${ESMC_TESTDIR}; ${OMAKE} -f $${dir}/makefile MAKEFILE=$${dir}/makefile testexamples_9
testexamples_10: ${TESTEXAMPLES_10}
testexamples_11: ${TESTEXAMPLES_11}
testexamples_12: ${TESTEXAMPLES_12}
testexamples_13: ${TESTEXAMPLES_13}

buildexamples_1: ${BUILDEXAMPLES_1}
buildexamples_2: ${BUILDEXAMPLES_2}
buildexamples_3: ${BUILDEXAMPLES_3}
buildexamples_4: ${BUILDEXAMPLES_4}
buildexamples_5: ${BUILDEXAMPLES_5}
buildexamples_6: ${BUILDEXAMPLES_6}
buildexamples_7: ${BUILDEXAMPLES_7}
buildexamples_8: ${BUILDEXAMPLES_8}
buildexamples_9: ${BUILDEXAMPLES_9}

