#  $Id: common.mk,v 1.103 2005/04/11 15:52:56 nscollins Exp $
#===============================================================================
#
#  GNUmake makefile - cannot be used with standard unix make!!
#
#  This file is included by all platforms and all builds, where all builds
#  include the ESMF Framework, the ESMF Implementation Report, and the
#  ESMF EVA codes.  Each of those builds has a separate ../makefile, so
#  any targets or rules which are specific to only a single build should
#  be in the top level makefile and not here.
#
#  If you have changes which only apply to a single platform, look in
#  ../build_config/<platform>/build_rules.mk  for the flags and libraries
#  which are included on a per-platform/compiler/specific-site basis. 
# 
#  Be very careful in making changes here; it is hard to make sure you
#  have not broken anything without testing all three build systems.
#  If you must, please look below for the comment section with the
#  label "HOWTO" before you dive in.
#
#===============================================================================

#-------------------------------------------------------------------------------
#  If environment variables are not set give them default values.
#  For some variables having the literal string 'default' is ok; 
#  for others, look for this string and override it the same as 
#  if it was unset originally.
#-------------------------------------------------------------------------------

ifndef ESMF_ARCH
ESMF_ARCH = default
endif
ifeq ($(ESMF_ARCH),default)
export ESMF_ARCH := $(shell uname -s)
endif

# -----------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------

# name of directory containing the generated files from the build
# defaults to the top dir, but can be set to be something different.
ifndef ESMF_BUILD
export ESMF_BUILD := $(ESMF_TOP_DIR)
endif

# make sure ESMF_COMM has a default value, even if it is the explicit 
# string 'default'
ifndef ESMF_COMM
ESMF_COMM = default
endif

# name of default compiler
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

# This is ok to remain as default
ifndef ESMF_SITE
export ESMF_SITE = default
endif

# Comment out the following lines if you want to include the IO code
# the cpp flags are automatically added to the fpp lines as well now.
#FPPFLAGS       += $(FPP_PREFIX)-DESMF_NO_IOCODE
CPPFLAGS       += -DESMF_NO_IOCODE
export ESMF_NO_IOCODE = true


# Conditionally turn off ESMF's pthread feature set and use pthread_stubs
ifndef ESMF_PTHREADS
export ESMF_PTHREADS = ON
endif
ifneq ($(ESMF_PTHREADS),ON)
export ESMF_PTHREADS = OFF
endif


# if PREC not already set, default to 64.  architectures which
# have only one word size set this variable in their compiler/platform
# dependent files, so this only applies to platforms which support
# more than one wordsize (e.g. ibm, irix).
ifndef ESMF_PREC
ESMF_PREC = default
endif
ifeq ($(ESMF_PREC),default)
export ESMF_PREC = 64
endif

#
#  ESMF_COMM set in site files, but if MPI_HOME has a default value
#  then use it for the compile flags. 
#
ifndef MPI_HOME
MPI_INCLUDE =
MPI_LIB     =
MPIRUN      = mpirun
else
MPI_INCLUDE = -I${MPI_HOME}/include
MPI_LIB     = -L${MPI_HOME}/lib
MPIRUN      = ${MPI_HOME}/bin/mpirun
endif

# if using PBS system, export this for run time
ifdef PBS_NODEFILE
export ESMF_NODES := -machinefile $(PBS_NODEFILE)
endif           

# default compiler flag is neither debug nor optimized.  can be set
# to g for debug, to O (capital oh) for optimized.
ifndef ESMF_BOPT
export ESMF_BOPT = 
endif
ifeq ($(ESMF_BOPT),default)
export ESMF_BOPT = 
endif


# common commands and flags.  override in the platform specific include
# files if they differ.
AR		   = ar
AR_FLAGS	   = cr
AR_EXTRACT         = -x
AR32_64            = ${AR}
RM		   = rm -f
RANLIB		   = ranlib
M4	           = m4

OMAKE		   = ${MAKE}
SHELL		   = /bin/sh
SED		   = /bin/sed

SL_SUFFIX          = so
SL_LIBS_TO_MAKE    = libesmf

C_FC_MOD           = -I
C_CLINKER          = ${C_CXX}
C_FLINKER          = ${C_FC}
C_LINKOPTS         = -L$(ESMF_LIBDIR)
C_SLFLAG           = -Wl,-rpath,
C_SL_LIBLINKER     = ${C_CXX}
C_SL_LIBOPTS       = 


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

LDIR		= $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)

ESMF_LIBDIR     = $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_MODDIR     = $(ESMF_BUILD)/mod/mod${ESMF_BOPT}/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_TESTDIR    = $(ESMF_BUILD)/test/test$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_EXDIR      = $(ESMF_BUILD)/examples/examples$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_INCDIR     = $(ESMF_BUILD)/src/include
ESMF_DOCDIR	= $(ESMF_TOP_DIR)/doc
ESMF_BUILD_DOCDIR = $(ESMF_BUILD)/build/doc
ESMF_STDIR      = $(ESMF_TOP_DIR)/src/system_tests

# (why are there both ESMC_ and ESMF_ files here??)
ESMC_OBJDIR	= ${ESMF_MODDIR}
ESMC_TESTDIR    = ${ESMF_TESTDIR}
ESMC_DOCDIR	= ${ESMF_DOCDIR}

ESMF_TEMPLATES	= ${ESMF_TOP_DIR}/scripts/doc_templates/templates
PROTEX		= ${ESMF_TEMPLATES}/protex 
CC_PROTEX       = ${ESMF_TEMPLATES}/scripts/do_ccprotex 
CH_PROTEX       = ${ESMF_TEMPLATES}/scripts/do_chprotex 
F_PROTEX        = ${ESMF_TEMPLATES}/scripts/do_fprotex 
DO_LATEX	= ${ESMF_TEMPLATES}/scripts/do_latex
DO_L2H		= ${ESMF_TEMPLATES}/scripts/do_l2h

CONFIG_TESTS    = ${ESMF_TESTDIR}/tests.config
ESMF_TESTSCRIPTS    = ${ESMF_TOP_DIR}/scripts/test_scripts
DO_UT_RESULTS	    = ${ESMF_TESTSCRIPTS}/do_ut_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -b $(ESMF_BOPT)
DO_EX_RESULTS	    = ${ESMF_TESTSCRIPTS}/do_ex_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_EXDIR) -b $(ESMF_BOPT)
DO_ST_RESULTS	    = ${ESMF_TESTSCRIPTS}/do_st_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -b $(ESMF_BOPT)
DO_SUM_RESULTS	    = ${ESMF_TESTSCRIPTS}/do_summary.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -e $(ESMF_EXDIR) -b $(ESMF_BOPT)

# set up the defaults for all compilers, all options.  if the platform
# dependent files want to add flags, they can += more flags.  if they want
# to override these flags, they can simply reassign them.

# debug option
G_CFLAGS = -g
G_FFLAGS = -g

# either debug nor optimized option
X_CFLAGS =
X_FFLAGS =

# optimize option
O_CFLAGS = -O
O_FFLAGS = -O

#-------------------------------------------------------------------------------
#  Set up all defaults before here.   Next, include both the system dependent
#  makefile fragment, and if present, the site-specific makefile fragment.
#  These files can += to add on to existing defaults, or override settings
#  by setting flags and variables with = to new values.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#  Include the default platform-specific makefile fragment.
#-------------------------------------------------------------------------------

include $(ESMF_TOP_DIR)/build_config/$(ESMF_ARCH).$(ESMF_COMPILER).default/build_rules.mk


#-------------------------------------------------------------------------------
#  Include site specific makefile fragment.  If we want to suppress a warning
#  message if the site file is not found add a leading dash before the include
#  keyword.  (If the file is not found it is a warning, not a fatal error.)
#-------------------------------------------------------------------------------

ifneq ($(ESMF_SITE),default)
include $(ESMF_TOP_DIR)/build_config/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_SITE)/build_rules.mk
endif

#-------------------------------------------------------------------------------
#  Now all system-dependent files have been read.  Variables which are C_xxx
#  are set on a per-platform basic in the makefile fragments.  Now anything
#  below here is again common code.  Variables should no longer be overwritten
#  with =, but should be appended to if neeeded with +=
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# The ESMF_COMM variable is valid and can be used now.
#
# the default link options if you are compiling with the mpiuni bypass library.

ifeq ($(ESMF_COMM),mpiuni)
CPPFLAGS       += -DESMF_MPIUNI

MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPI_LIB        = -lmpiuni
MPIRUN         = ${MPI_HOME}/mpirun
endif

#-------------------------------------------------------------------------------


# alternate strategy:
#ifdef ESMF_HAVE_BLAS  
# do stuff here to add includes and libs to flags
# disadvantage: need extra #defines
# advantage: you can define default values for the lib and include vars
#   without them being used if the controlling ifdef is not defined.
# but i suspect that the location of these libs is highly system dependent
# if they are not in /usr/lib or /usr/local/lib, so trying to guess where
# they are is a losing proposition.  go with single defines for now.
#endif

ifdef MP_LIB
EXTRA_INCLUDES += $(MP_INCLUDE)
EXTRA_LIBS += $(MP_LIB)
endif

ifdef THREAD_LIB
EXTRA_INCLUDES += $(THREAD_INCLUDE)
EXTRA_LIBS += $(THREAD_LIB)
endif

ifdef BLAS_LIB
EXTRA_INCLUDES += $(BLAS_INCLUDE)
EXTRA_LIBS += $(BLAS_LIB)
endif

ifdef LAPACK_LIB
EXTRA_INCLUDES += $(LAPACK_INCLUDE)
EXTRA_LIBS += $(LAPACK_LIB)
endif

ifdef ESSL_LIB
EXTRA_INCLUDES += $(ESSL_INCLUDE)
EXTRA_LIBS += $(ESSL_LIB)
endif

ifdef PCL_LIB
EXTRA_INCLUDES += $(PCL_INCLUDE)
EXTRA_LIBS += $(PCL_LIB)
endif

ifdef HDF_LIB
EXTRA_INCLUDES += $(HDF_INCLUDE)
EXTRA_LIBS += $(HDF_LIB)
endif

# netcdf calls are always referenced from the esmf library, so if the user
# does not have them, we supply a stub library which simply prints an error
# if they are called.
ifndef NETCDF_LIB
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
NETCDF_LIB       = -lnetcdf_stubs
endif

# at this point netcdf always has a value - either from the user pointing
# to the real lib location, or our stub lib.
EXTRA_INCLUDES += $(NETCDF_INCLUDE)
EXTRA_LIBS += $(NETCDF_LIB)


ifeq ($(ESMF_PTHREADS),OFF)
CPPFLAGS       += -DESMF_NO_PTHREADS
endif
# this is needed even if compiling with pthreads on
EXTRA_INCLUDES += -I${ESMF_DIR}/src/Infrastructure/stubs/pthread

# does this actually get used?   seems not.
#-------------------------------------------------------------------------------
# Defines all libraries needed for using linear and nonlinear solvers.
# The order of listing these libraries is important!
#
# PCONF - indicates which OPTIONAL external packages are available at your site
#-------------------------------------------------------------------------------
#
#PCONF		= ${ESMC_HAVE_MPE}  ${ESMC_HAVE_PARMETIS} \
#                  ${ESMC_HAVE_AMS}  ${ESMC_HAVE_X11}   ${ESMC_HAVE_MATLAB} \
#                  ${ESMC_HAVE_ADIC} ${ESMC_HAVE_JAVA}
#EXTERNAL_LIB	= ${MPE_LIB}        ${BLOCKSOLVE_LIB}  ${PARMETIS_LIB} \
#                  ${AMS_LIB}        ${SPAI_LIB} \
#                  ${ADIC_LIB} 
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#

LIBNAME		= $(ESMF_LIBDIR)/${LIBBASE}.a
ESMFLIB		= $(ESMF_LIBDIR)/libesmf.a

SOURCE		= ${SOURCEC} ${SOURCEF}
OBJS		= ${OBJSC} ${OBJSF}


ESMC_INCLUDE	= -I${ESMF_TOP_DIR}/${LOCDIR} \
		  -I${ESMF_TOP_DIR}/${LOCDIR}/../include \
		  ${LOCAL_INCLUDE} \
		  -I${ESMF_TOP_DIR}/build_config/${ESMF_ARCH}.$(ESMF_COMPILER).$(ESMF_SITE) \
		  -I$(ESMF_INCDIR) -I$(ESMF_MODDIR) $(MPI_INCLUDE) \
                  ${EXTRA_INCLUDES}

CCPPFLAGS	+= ${PCONF} ${ESMC_PARCH} ${CPPFLAGS} -D__SDIR__='"${LOCDIR}"'
FCPPFLAGS	+= ${PCONF} ${ESMC_PARCH} ${FPPFLAGS} 

# these seem unused.  TODO remove.
#C_SH_LIB_PATH	= ${SLFLAG}${LDIR} ${C_DYLIBPATH}
#F_SH_LIB_PATH	= ${SLFLAG}${LDIR} ${F_DYLIBPATH}

ESMC_TIME_LIB	 = -L${LDIR}

#-------------------------------------------------------------------------------
#  Common variable definitions.
#-------------------------------------------------------------------------------

CC             = ${C_CC}
CXX	       = ${C_CXX}
FC	       = ${C_FC}

FC_MOD         = ${C_FC_MOD}

# unless CPP has already been defined in the included platform specific
# makefile, override it here (it defaults to cc, which does not have all
# the functions needed by our preprocessed code.)
ifneq ($(origin CPP), file)
CPP	       = gcc
endif

#-------------------------------------------------------------------------------
# add in any FPPOPTS defined in the system dep files, and add a definition
#  for the selected word size (32/64) by defining the syms S32 or S64.
#-------------------------------------------------------------------------------
CPPFLAGS        +=-DS${ESMF_PREC}=1 

FPPFLAGS        += $(addprefix $(FPP_PREFIX), $(C_FPPOPTS))
FPPFLAGS        += $(addprefix $(FPP_PREFIX), $(CPPFLAGS))


#-------------------------------------------------------------------------------
# ESMF_EXHAUSTIVE is passed (by CPP) into test programs to control the number 
# of tests that a test program will do.
#-------------------------------------------------------------------------------

ifeq ($(ESMF_EXHAUSTIVE),ON) 
# the cpp flags are automatically added to the fpp lines as well now.
#FPPFLAGS       += $(FPP_PREFIX)-DESMF_EXHAUSTIVE 
CPPFLAGS       += -DESMF_EXHAUSTIVE 
endif


# C++ <=> F90 
F90CXXLIBS     = ${C_F90CXXLIBS}
CXXF90LIBS     = ${C_CXXF90LIBS}
# unused?
#CXXSO          = ${C_CXXSO}
#CXXSOLIBS      = ${C_CXXSOLIBS}
ESMC_LANGUAGE = CONLY
ESMC_SCALAR   = real

#-------------------------------------------------------------------------------
# Variable definitions for debug option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),g)
COPTFLAGS      = ${G_CFLAGS}
FOPTFLAGS      = ${G_FFLAGS}
endif

#-------------------------------------------------------------------------------
# Variable definitions for non-debug, non-optimized option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),)
COPTFLAGS      = ${X_CFLAGS}
FOPTFLAGS      = ${X_FFLAGS}
endif

#-------------------------------------------------------------------------------
# Variable definitions for optimized option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),O)
COPTFLAGS      = ${O_CFLAGS}
FOPTFLAGS      = ${O_FFLAGS}
endif


#-------------------------------------------------------------------------------
# Now set the loader and linkers to use the selected set of flags.
#-------------------------------------------------------------------------------
# only used by impl report, as far as i can tell.  TODO: remove this
#CXXF90LD       = ${C_CXXF90LD} -L$(ESMF_LIBDIR)
#F90CXXLD       = ${C_F90CXXLD} -L$(ESMF_LIBDIR)

# building a shared lib.so from a lib.a
SL_LIBLINKER = ${C_SL_LIBLINKER}
SL_LIBOPTS   = ${C_SL_LIBOPTS}

# linking executables, taking into account libesmf might be shared
SLFLAG       = ${C_SLFLAG}

CLINKER      = ${C_CLINKER}
FLINKER      = ${C_FLINKER}
LINKOPTS     = ${C_LINKOPTS}

# original lines - do these options need to be here?
#FLINKER       = ${C_FLINKER} ${FOPTFLAGS} ${F_SH_LIB_PATH}
#CLINKER       = ${C_CLINKER} ${COPTFLAGS} ${C_SH_LIB_PATH}

# default is to include the esmf lib dir in the library search path
LIB_PATHS = -L$(LDIR)
LD_PATHS  = $(SLFLAG)$(LDIR)

# then append each directory which is in LD_LIBRARY_PATH to
# the -L flag and also to the run-time load flag.  (on systems which
# support the 'module' command, that is how it works - by adding dirs
# to LD_LIBRARY_PATH.)  if your libs are not found, set LD_LIBRARY_PATH,
# or make a site specific file and edit the paths explicitly.
ifeq ($(origin LD_LIBRARY_PATH), environment)
LIB_PATHS  += $(addprefix -L, $(subst :, ,$(LD_LIBRARY_PATH)))
LD_PATHS   += $(addprefix $(SLFLAG), $(subst :, ,$(LD_LIBRARY_PATH)))
endif



#-------------------------------------------------------------------------------
# HOWTO:  Warning: Here there be dragons.
# 
# There are 3 separate top level makefiles: for the ESMF Framework, for the
# ESMF Implementation Report, and for the ESMF EVA (Validation) codes.
# There is this file (build/common.mk), there are platform-dependent makefiles
# (build_config/<platform+compiler>/build_rules.mk), and there are
# makefiles in each subdir.  Needless to say, this makes things confusing
# when trying to decide where to make changes.  
# 
# Here are a few things to know about targets in this file:
# 
# If you need to make a new target which should be called in each
# of the possible source subdirectories, you will typically have to
# add at least two targets:  "fred:" and "tree_fred:".  The plain
# target is the one you invoke, and it should look like this:
# 
# fred:
# 	cd $(ESMF_TOP_DIR) ;\
# 	$(MAKE) ACTION=tree_fred tree
# 
# "tree" is a preexisting target in this file which recursively descends 
# the build tree (using the DIR= settings in each individual makefile 
# to know which subdirs to descend into), and it calls 'make $ACTION' 
# in each of the subdirs.   Since you do not want to replicate the target
# in each of the 100s of individual makefiles, typically you put the
# tree_target here in this file as well, and it feeds off variables
# which are set in the individual makefiles (e.g. SOURCEF, CLEANDIRS, etc).
# Look at some of the existing tree_<xxx> rules for ideas on how to
# add new targets.
# 
# If you need to make a target which does not work on every subdirectory
# you can still put the target here, but do not change to the top level
# dir before executing the rule.
# 
# Since the "clean" and "clobber" targets remove directories that are needed
# the next time you build, there are chkdir_<fred> targets which ensure the 
# directories are created first, so the individual rules which follow can 
# just assume that those directories succeed.  Notice that the rules use
# the -p option on mkdir which ensures intermediate directories are created
# if they do not exist.
# 
# Some of the library rules below are complicated by the fact that some
# compilers will not let you control where .mod fortran module files are
# created; they are created in the current directory.  Rather than try to
# copy them into the target directory (which can be complicated by some
# systems making .MOD files and some making .mod files), we instead cd into
# the mod directory and then compile from there using full pathnames.
# This also ensures that if multiple builds are running for different
# target compilers they do not interfere with each other since each mod
# directory is separate based on the platform and compiler.
# 
# Another complication: we have to support the ability to build in a
# different tree than the source files.  This might be used if the user
# has a shared copy of the source checked out and does not have write
# permission in those directories, or if the output has to appear in a
# different set of directories, for example to merge with a larger build
# system.  Users can set ESMF_BUILD to another location.  Any files which
# are created should use variables which feed off ESMF_BUILD for output,
# and use ESMF_TOP_DIR for files which were checked out of CVS and only
# used as input.  ESMF_TOP_DIR is set to be ESMF_DIR for the framework
# and EVA builds, and to ESMF_IMPL_DIR for the Implementation Report.
# ESMF_BUILD always defaults to the same location as ESMF_TOP_DIR.
#
# good luck.
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Create various directories where files expect to be copied into.
#-------------------------------------------------------------------------------
chk_dir:
	-@if [ ! -d $(ESMF_LIBDIR) ]; then \
	  echo Making directory $(ESMF_LIBDIR) for library; \
	  mkdir -p $(ESMF_LIBDIR) ; fi
	-@if [ ! -d ${ESMF_MODDIR} ]; then \
	  echo Making directory ${ESMF_MODDIR} for *.mod files; \
	  mkdir -p ${ESMF_MODDIR} ; fi

chkdir_doc:
	-@if [ ! -d ${ESMC_DOCDIR} ]; then \
	  echo Making directory ${ESMC_DOCDIR} for documents; \
	  mkdir -p ${ESMC_DOCDIR} ; fi

chkdir_tests:
	-@if [ ! -d ${ESMF_TESTDIR} ]; then \
	  echo Making directory ${ESMF_TESTDIR} for test output; \
	  mkdir -p ${ESMF_TESTDIR} ; fi

chkdir_include:
	-@if [ ! -d $(ESMF_INCDIR) ]; then \
	  echo Making directory $(ESMF_INCDIR) for include files; \
	  mkdir -p $(ESMF_INCDIR) ; fi

chkdir_examples:
	-@if [ ! -d ${ESMF_EXDIR} ]; then \
	  echo Making directory ${ESMF_EXDIR} for examples output; \
	  mkdir -p ${ESMF_EXDIR} ; fi


#-------------------------------------------------------------------------------
# This target used to check that variables which had to have settings
# were indeed set.  All have been removed now, but this target is still
# here keep from breaking other dependency rules.  At some point it can
# go away.
chkopts:
	@echo ""

# Does nothing; needed for some rules that require actions.
foo:

#-------------------------------------------------------------------------------
# Builds ESMF recursively.
#-------------------------------------------------------------------------------

# this is a magic gnumake variable which helps it find files.
VPATH = ${ESMF_TOP_DIR}/${LOCDIR}:${ESMF_TOP_DIR}/include

libc:${LIBNAME}(${OBJSC})
libf:${LIBNAME}(${OBJSF})

# Build all of ESMF from the top.  This target can be called from any
# subdir and it will go up to the top dir and build from there.
build_libs: chk_dir include cppfiles
	cd $(ESMF_TOP_DIR) ;\
	${MAKE} ACTION=tree_lib tree shared

# Build only stuff in and below the current dir.
build_here: chk_dir
	${MAKE} ACTION=tree_lib tree shared

# Builds library - action for the 'tree' target.
tree_lib:
	dir=`pwd`; cd ${ESMC_OBJDIR}; ${MAKE} -f $${dir}/makefile MAKEFILE=$${dir}/makefile lib

# Builds library
lib:: chk_dir ${SOURCE}
	@if [ "${SOURCEC}" != "" ] ; then \
	   $(MAKE) -f ${MAKEFILE} libc; fi
	@if [ "${SOURCEF}" != "" ] ; then \
	    $(MAKE) -f ${MAKEFILE}  libf; fi
	@if [ "${OBJS}" != " " ] ; then \
		${RANLIB} ${LIBNAME}; \
		${RM} -f ${OBJS}; \
	fi
	@if [ "${QUICKSTART}" != "" ] ; then \
	   $(MAKE) -f ${MAKEFILE} tree_build_quick_start; fi


# copy private include files into src/include directory.
include: chkdir_include
	cd $(ESMF_TOP_DIR) ;\
	$(MAKE) ACTION=tree_include tree

# action for 'tree' target.
tree_include:
	-@for hfile in ${STOREH} foo ; do \
	  if [ $$hfile != "foo" ]; then \
	    cp -f ../include/$$hfile $(ESMF_INCDIR) ; \
	  fi ; \
	done

# create .F90 source files from .cpp files.
cppfiles: chkdir_include include
	cd $(ESMF_TOP_DIR) ;\
	$(MAKE) ACTION=tree_cppfiles tree

# action for 'tree' target.
tree_cppfiles:  $(CPPFILES)

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
# ------------------------------------------------------------------------------

# default list of files and dirs to clean (and surprisingly to me, 
# you cannot enclose these in quotes - they are preserved and the quotes
# prevent the wildcards from being expanded.)
CLEAN_DEFDIRS = coredir.*
CLEAN_DEFAULTS = *.o *.mod *.txt *.stdout NULL core UTestLog *ESMF_LogFile
CLEAN_TEXFILES = *.aux *.bbl *.blg *.log *.toc *.dvi *.ORIG

clean:
	$(MAKE) ACTION=tree_clean tree


clobber: clean
	@for DIR in $(CLOBBERDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      $(RM) -r $$DIR ;\
	   fi ;\
	done


# action for 'tree' target.
tree_clean:
	@for DIR in $(CLEANDIRS) $(CLEAN_DEFDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      $(RM) -r $$DIR ;\
	   fi ;\
	done
	$(RM) $(CLEANFILES) $(CLEAN_DEFAULTS)

# target which does a light cleaning - remove files only under the src dir 
#  (logfiles, doc files, test output files, files made by preprocessing, etc)
#  leaves the libs, executables, etc alone.
dust:
	@cd $(ESMF_BUILD)/src ;\
	$(MAKE) ACTION=tree_dust tree

tree_dust:
	@for DIR in $(DUSTDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      cd $$DIR; $(MAKE) ACTION=tree_clean tree ;\
	   fi ;\
	done
	
#-------------------------------------------------------------------------------
# Targets for building and running system tests.
#-------------------------------------------------------------------------------

system_tests: chkopts build_libs chkdir_tests
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR); fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
               echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
               exit; \
	   fi; \
        fi; \
	$(MAKE) ACTION=tree_system_tests tree
	$(MAKE) check_system_tests

tree_system_tests: tree_build_system_tests tree_run_system_tests

#
# system_tests_uni, build and run uni versions of the system tests
#
system_tests_uni: chkopts chkdir_tests
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR); fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
	fi; \
	$(MAKE) ACTION=tree_system_tests_uni tree
	$(MAKE) check_system_tests

tree_system_tests_uni: tree_build_system_tests tree_run_system_tests_uni

#
# build_system_tests
#
build_system_tests: chkopts chkdir_tests
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR) ; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
        fi; \
	$(MAKE) ACTION=tree_build_system_tests tree

tree_build_system_tests:  $(SYSTEM_TESTS_BUILD) 

#
#  Link rule for Fortran system tests.
#
$(ESMC_TESTDIR)/ESMF_%STest : ESMF_%STest.o $(SYSTEM_TESTS_OBJ) $(ESMFLIB)
	-$(FLINKER) $(LINKOPTS) -o $@ $(SYSTEM_TESTS_OBJ) $< -lesmf \
        ${MPI_LIB} ${EXTRA_LIBS} ${F90CXXLIBS}
	${RM} -f *.o *.mod

#
# run_system_tests
#
run_system_tests:  chkopts chkdir_tests
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR) ; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
        fi; \
	$(MAKE) ACTION=tree_run_system_tests tree
	$(MAKE) check_system_tests

tree_run_system_tests: $(SYSTEM_TESTS_RUN) 

#
# run_system_tests_uni
#
run_system_tests_uni:  chkopts chkdir_tests
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR) ; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
        fi; \
	$(MAKE) ACTION=tree_run_system_tests_uni tree
	$(MAKE) check_system_tests

tree_run_system_tests_uni: $(SYSTEM_TESTS_RUN_UNI)

#
# report statistics on system tests
#
check_system_tests:
	$(DO_ST_RESULTS)


#-------------------------------------------------------------------------------
#  Targets for building and running unit tests.
#-------------------------------------------------------------------------------

unit_tests: chkopts chkdir_tests build_libs
	$(MAKE) MULTI="Multiprocessor" config_unit_tests
	-$(MAKE) ACTION=tree_unit_tests tree
	$(MAKE) check_unit_tests

tree_unit_tests: tree_build_unit_tests tree_run_unit_tests

#
# tests_uni
#
unit_tests_uni: chkopts chkdir_tests
	$(MAKE) MULTI="Uniprocessor" config_unit_tests
	-$(MAKE) ACTION=tree_unit_tests_uni tree
	$(MAKE) check_unit_tests

tree_unit_tests_uni: tree_build_unit_tests tree_run_unit_tests_uni

#
# build_unit_tests
#
build_unit_tests: chkopts chkdir_tests
	$(MAKE) MULTI="Multiprocessor" config_unit_tests
	-$(MAKE) ACTION=tree_build_unit_tests tree

tree_build_unit_tests: $(TESTS_BUILD)


$(ESMC_TESTDIR)/ESMF_%UTest : ESMF_%UTest.o $(ESMFLIB)
	-$(FLINKER) $(LINKOPTS) -o $@  $(UTEST_$(*)_OBJS) $< -lesmf \
        ${MPI_LIB} ${EXTRA_LIBS} ${F90CXXLIBS}
	${RM} -f *.o *.mod


$(ESMC_TESTDIR)/ESMC_%UTest : ESMC_%UTest.o $(ESMFLIB)
	-$(CLINKER) $(LINKOPTS) -o $@  $(UTEST_$(*)_OBJS) $< -lesmf \
        ${MPI_LIB} ${EXTRA_LIBS} ${CXXF90LIBS} 
	${RM} -f *.o *.mod


#
# run_unit_tests
#
run_unit_tests:  chkopts chkdir_tests
	-@if [ -f ${CONFIG_TESTS} ] ; then \
	   sed 's/ .*processor/ Multiprocessor/' ${CONFIG_TESTS} > ${CONFIG_TESTS}.temp; \
           mv -f ${CONFIG_TESTS}.temp ${CONFIG_TESTS}; \
        fi
	-$(MAKE) ACTION=tree_run_unit_tests tree
	$(MAKE) check_unit_tests

tree_run_unit_tests: $(TESTS_RUN) 

#
# run_unit_tests_uni
#
run_unit_tests_uni:  chkopts chkdir_tests
	-@if [ -f ${CONFIG_TESTS} ] ; then \
	   sed 's/ .*processor/ Uniprocessor/' ${CONFIG_TESTS} > ${CONFIG_TESTS}.temp; \
           mv -f ${CONFIG_TESTS}.temp ${CONFIG_TESTS}; \
        fi
	-$(MAKE) ACTION=tree_run_unit_tests_uni tree 
	$(MAKE) check_unit_tests

tree_run_unit_tests_uni: $(TESTS_RUN_UNI)

#
# echo into a file how the tests were built and run, so we can
# check them correctly.
#
config_unit_tests:
	-@echo "# This file used by test scripts, please do not delete." > ${CONFIG_TESTS}
ifeq ($(ESMF_EXHAUSTIVE),ON) 
	-@echo "Exhaustive " ${MULTI} >> ${CONFIG_TESTS}
else
	-@echo "Non-exhaustive " ${MULTI} >> ${CONFIG_TESTS}
endif

#
# report statistics on tests
#
check_unit_tests:
	$(DO_UT_RESULTS)

#-------------------------------------------------------------------------------
#  Obsolete targets for building and running unit tests.  Echo an error
#  and point users to updated target names.
#-------------------------------------------------------------------------------

.PHONY:  tests tests_uni build_tests run_tests run_tests_uni check_tests

tests: ; $(error Obsolete target, use unit_tests now)

tests_uni: ; $(error Obsolete target, use unit_tests_uni now)

build_tests: ; $(error Obsolete target, use build_unit_tests now)

run_tests: ; $(error Obsolete target, use run_unit_tests now)

run_tests_uni: ; $(error Obsolete target, use run_unit_tests_uni now)

check_tests: ; $(error Obsolete target, use check_unit_tests now)

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
	-$(MAKE) ACTION=tree_examples tree
	$(MAKE) check_examples


tree_examples: tree_build_examples tree_run_examples

#
# examples_uni
#
examples_uni: chkopts chkdir_examples  
	-$(MAKE) ACTION=tree_examples_uni tree
	$(MAKE) check_examples

tree_examples_uni: tree_build_examples tree_run_examples_uni

#
# build_examples
#
build_examples: chkopts chkdir_examples
	-$(MAKE) ACTION=tree_build_examples tree

tree_build_examples: $(EXAMPLES_BUILD) 

#
#  Examples Link commands
#
$(ESMF_EXDIR)/ESMF_%Ex : ESMF_%Ex.o $(ESMFLIB)
	-$(FLINKER) $(LINKOPTS) -o $@ $(EXAMPLE_$(*)_OBJS) $< -lesmf \
        ${MPI_LIB} ${EXTRA_LIBS} ${F90CXXLIBS}
	${RM} -f *.o *.mod


$(ESMF_EXDIR)/ESMC_%Ex: ESMC_%Ex.o $(ESMFLIB)
	-$(CLINKER) $(LINKOPTS) -o $@ $(EXAMPLE_$(*)_OBJS) $< -lesmf \
        ${MPI_LIB} ${EXTRA_LIBS} ${CXXF90LIBS}
	$(RM) $<

#
# run_examples
#
run_examples:  chkopts chkdir_examples
	-$(MAKE) ACTION=tree_run_examples tree
	$(MAKE) check_examples

tree_run_examples: $(EXAMPLES_RUN) 


# run_examples_uni
#
run_examples_uni:  chkopts chkdir_examples
	-$(MAKE) ACTION=tree_run_examples_uni tree 
	$(MAKE) check_examples

tree_run_examples_uni: $(EXAMPLES_RUN_UNI)

#
# report statistics on examples
#
check_examples:
	$(DO_EX_RESULTS)


#-------------------------------------------------------------------------------
# Targets for building and running demos.
#-------------------------------------------------------------------------------

demos: chkopts build_libs chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_demos tree

tree_demos: tree_build_demos tree_run_demos

demos_uni: chkopts build_libs chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_demos_uni tree

tree_demos_uni: tree_build_demos tree_run_demos_uni

#
# build_demos
#
build_demos: chkopts chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_build_demos tree

tree_build_demos: $(DEMOS_BUILD) 

$(ESMC_TESTDIR)/%App : %Demo.o $(DEMOS_OBJ) $(ESMFLIB)
	$(FLINKER) $(LINKOPTS) -o $@ $(DEMOS_OBJ) $< -lesmf ${MPI_LIB} \
	${EXTRA_LIBS} ${F90CXXLIBS}
	${RM} -f *.o *.mod


#
# run_demos
#
run_demos:  chkopts chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_run_demos tree

tree_run_demos: $(DEMOS_RUN) 

run_demos_uni:  chkopts chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_run_demos_uni tree

tree_run_demos_uni: $(DEMOS_RUN_UNI) 


#-------------------------------------------------------------------------------
# Targets for checking the builds
#-------------------------------------------------------------------------------

check_results: check_tests check_examples check_system_tests

results_summary:
	$(DO_SUM_RESULTS)


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
	$(MAKE) ACTION=tree_build_release tree

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
	$(MAKE) ACTION=tree_build_quick_start tree

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

doc:  chkdir_doc
	-@echo "========================================="
	-@echo "doc rule from common.mk"
	-@echo "========================================="
	cd $(ESMF_TOP_DIR)/src/doc ;\
	$(MAKE) dvi html pdf
	-@echo "Build doc completed."

alldoc: chkdir_doc include cppfiles tex
	-@echo "========================================="
	-@echo "Building All Documentation"
	-@echo "========================================="
	-@$(MAKE) dvi pdf html
	-@echo "Build alldoc completed."

tex: chkdir_doc include cppfiles
	cd $(ESMF_TOP_DIR) ;\
	$(MAKE) ACTION=tree_tex tree

tree_tex: $(TEXFILES_TO_MAKE)

dvi: chkdir_doc include cppfiles tex
	-@echo "========================================="
	-@echo "dvi rule from common.mk, Building .dvi files"
	-@echo "dvi files are:" $(DVIFILES)
	-@echo "========================================="
	$(MAKE) $(DVIFILES)

tree_dvi: chkdir_doc ${DVIFILES}


pdf: chkdir_doc
	-@echo "========================================="
	-@echo "pdf rule from common.mk, Building .pdf files"
	-@echo "pdf files are:" $(PDFFILES)
	-@echo "========================================="
	$(MAKE) $(PDFFILES)

tree_pdf: chkdir_doc ${PDFFILES}


html: chkdir_doc include cppfiles tex
	-@echo "========================================="
	-@echo "html rule from common.mk, Building .html files"
	-@echo "html files are:" $(HTMLFILES)
	-@echo "========================================="
	$(MAKE) $(HTMLFILES)

tree_html:chkdir_doc ${HTMLFILES}

clean_doc:
	@cd $(ESMF_BUILD)/src/doc ;\
	$(MAKE) tree_clean 

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

# i suspect that the moddir needs to be given to these first 2 because
# unlike the libs, we compile the system tests and examples in their
# own directories without cding first to the test dir?

.F90.o:
	${FC} -c ${FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} \
           ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.F.o:
	${FC} -c ${FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} \
           ${F_FREENOCPP} $<

.f90.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} \
           ${F_FIXCPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<

.f.o:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXNOCPP} $<

.c.o:
	${CC} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.C.o:
	${CXX} -c ${COPTFLAGS} ${CFLAGS} ${CCPPFLAGS} ${ESMC_INCLUDE} $<

.F90.a:
	${FC} -c ${FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} \
           ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.F.a:
	${FC} -c ${FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} \
           ${F_FREENOCPP} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f90.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} \
           ${F_FIXCPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $<
	${AR} ${AR_FLAGS} ${LIBNAME} $*.o
	${RM} $*.o

.f.a:
	${FC} -c ${FOPTFLAGS} ${FFLAGS} ${F_FIXNOCPP} $<
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
ifeq ($(origin CPPRULES),undefined)
.cpp.F90:
	${CPP} -E -P -I${ESMF_INCDIR} $< | tr "@^" "\n#" | \
              sed -e '/^#pragma GCC/d' > $(dir $<)$(notdir $@)


.cpp.o:
	${CPP} -E -P -I${ESMF_INCDIR} $< | tr "@^" "\n#" | \
              sed -e '/^#pragma GCC/d' > $(dir $<)$(basename $@).F90
	${FC} -c ${FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} \
          ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $(dir $<)$(basename $@).F90
endif


#-------------------------------------------------------------------------------
#  Build shared library from regular lib (.so from .a)
#-------------------------------------------------------------------------------
shared:
	@if [ "${SL_LIBS_TO_MAKE}" != "" ] ; then \
		echo making shared libraries in $(LDIR); \
		cd $(LDIR) ; \
		$(RM) -r tmp_* ; \
		for NEXTLIB in $(SL_LIBS_TO_MAKE) foo ;\
		do \
		if [ -f $$NEXTLIB.a ] ; then \
		    $(RM) $$NEXTLIB.$(SL_SUFFIX) ; \
		    echo Converting $$NEXTLIB.a to $$NEXTLIB.$(SL_SUFFIX) ;\
		    mkdir tmp_$$NEXTLIB ;\
		    cd tmp_$$NEXTLIB  ;\
	            $(AR) $(AR_EXTRACT) ../$$NEXTLIB.a ;\
		    $(SL_LIBLINKER) $(SL_LIBOPTS) -o $(LDIR)/$$NEXTLIB.$(SL_SUFFIX) *.o ;\
		    cd .. ;\
		    $(RM) -r tmp_$$NEXTLIB ;\
		fi ;\
		done ; \
	fi \

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
export TEXINPUTS_VALUE


#-------------------------------------------------------------------------------
#  dvi rules
#-------------------------------------------------------------------------------
%_desdoc.dvi : %_desdoc.ctex $(DESDOC_DEP_FILES)
	-@echo "========================================="
	-@echo "_desdoc.dvi rule from common.mk"
	-@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* des

%_refdoc.dvi : %_refdoc.ctex $(REFDOC_DEP_FILES)
	-@echo "========================================="
	-@echo "_refdoc.dvi rule from common.mk"
	-@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* ref

%_reqdoc.dvi : %_reqdoc.ctex $(REQDOC_DEP_FILES)
	-@echo "========================================="
	-@echo "_reqdoc.dvi rule from common.mk"
	-@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* req

#-------------------------------------------------------------------------------
#  pdf rules
#-------------------------------------------------------------------------------

$(ESMC_DOCDIR)/%.pdf: %.dvi
	-@echo "========================================="
	-@echo "_%pdf from %.dvi rule from common.mk"
	-@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	dvipdf $< $@

#-------------------------------------------------------------------------------
#  html rules
#-------------------------------------------------------------------------------
$(ESMC_DOCDIR)/%_desdoc: %_desdoc.ctex $(DESDOC_DEP_FILES)
	-@echo "========================================="
	-@echo "_%desdoc from %.ctex rule from common.mk"
	-@echo "========================================="
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} $* des
	$(RM) .latex2html-init
	mv -f $(@F) $(ESMC_DOCDIR)


$(ESMC_DOCDIR)/%_refdoc: %_refdoc.ctex $(REFDOC_DEP_FILES)
	-@echo "========================================="
	-@echo "_%refdoc from %.ctex rule from common.mk"
	-@echo "========================================="
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} $* ref
	$(RM) .latex2html-init
	mv -f $(@F) $(ESMC_DOCDIR)

$(ESMC_DOCDIR)/%_reqdoc: %_reqdoc.ctex $(REQDOC_DEP_FILES)
	-@echo "========================================="
	-@echo "_%reqdoc from %.ctex rule from common.mk"
	-@echo "========================================="
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} $* req
	$(RM) .latex2html-init
	mv -f $(@F) $(ESMC_DOCDIR)

#-------------------------------------------------------------------------------
#  These rules are for compiling the test examples.
#-------------------------------------------------------------------------------
.cpp.rm .cc.rm .C.rm .F.rm .f.rm .c.rm:
	-@${RM} $* *.o $*.mon.* gmon.out mon.out


#-------------------------------------------------------------------------------
# Keep .o files
#-------------------------------------------------------------------------------
.PRECIOUS: %.o



