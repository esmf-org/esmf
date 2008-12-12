#  $Id: common.mk,v 1.201.2.23 2008/12/12 18:24:09 theurich Exp $
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
# Test for obsolete environment variables, print error and stop build here
#-------------------------------------------------------------------------------

ifeq ($(origin ESMF_ARCH), environment)
$(error Obsolete environment variable ESMF_ARCH detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_PREC), environment)
$(error Obsolete environment variable ESMF_PREC detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_TOP_DIR), environment)
$(error Obsolete environment variable ESMF_TOP_DIR detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_NODES), environment)
$(error Obsolete environment variable ESMF_NODES detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_C_COMPILER), environment)
$(error Obsolete environment variable ESMF_C_COMPILER detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_C_LIBRARY), environment)
$(error Obsolete environment variable ESMF_C_LIBRARY detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_CXX_LIBRARY_PATH), environment)
$(error Obsolete environment variable ESMF_CXX_LIBRARY_PATH detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_CXX_LIBRARIES), environment)
$(error Obsolete environment variable ESMF_CXX_LIBRARIES detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_COMPILER_VERSION), environment)
$(error Obsolete environment variable ESMF_COMPILER_VERSION detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_STDCXX_LIBRARY), environment)
$(error Obsolete environment variable ESMF_STDCXX_LIBRARY detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_F90_LIBRARY_PATH), environment)
$(error Obsolete environment variable ESMF_F90_LIBRARY_PATH detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_F90_LIBRARIES), environment)
$(error Obsolete environment variable ESMF_F90_LIBRARIES detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_NO_LD_LIBRARY_PATH), environment)
$(error Obsolete environment variable ESMF_NO_LD_LIBRARY_PATH detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_PROJECT), environment)
$(error Obsolete environment variable ESMF_PROJECT detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_LIB_INSTALL), environment)
$(error Obsolete environment variable ESMF_LIB_INSTALL detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_MOD_INSTALL), environment)
$(error Obsolete environment variable ESMF_MOD_INSTALL detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_H_INSTALL), environment)
$(error Obsolete environment variable ESMF_H_INSTALL detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_NO_IOCODE), environment)
$(error Obsolete environment variable ESMF_NO_IOCODE detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_EXHAUSTIVE), environment)
$(error Obsolete environment variable ESMF_EXHAUSTIVE detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_BATCH), environment)
$(error Obsolete environment variable ESMF_BATCH detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_BATCHOPTIONS), environment)
$(error Obsolete environment variable ESMF_BATCHOPTIONS detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_MPI), environment)
$(error Obsolete environment variable ESMF_MPI detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin ESMF_MPIRUNOPTIONS), environment)
$(error Obsolete environment variable ESMF_MPIRUNOPTIONS detected. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

ifeq ($(origin MPI_HOME), environment)
$(warning Environment variable MPI_HOME detected. ESMF no longer uses this environment variable. Please see ESMF README and/or User's Guide for a current list of ESMF environment variables.)
endif

#-------------------------------------------------------------------------------
# Set defaults for environment variables that are not set
#-------------------------------------------------------------------------------

ifndef ESMF_BUILD
export ESMF_BUILD = default
endif

ifndef ESMF_OS
export ESMF_OS = default
endif

ifndef ESMF_MACHINE
export ESMF_MACHINE = default
endif

ifndef ESMF_ABI
export ESMF_ABI = default
endif

ifndef ESMF_COMM
export ESMF_COMM = default
endif

ifndef ESMF_COMPILER
export ESMF_COMPILER = default
endif

ifndef ESMF_BOPT
export ESMF_BOPT = default
endif

ifndef ESMF_OPTLEVEL
export ESMF_OPTLEVEL = default
endif

ifndef ESMF_SITE
export ESMF_SITE = default
endif

ifndef ESMF_PTHREADS
export ESMF_PTHREADS = $(ESMF_PTHREADSDEFAULT)
endif

ifndef ESMF_ARRAY_LITE
export ESMF_ARRAY_LITE = default
endif

ifndef ESMF_NO_INTEGER_1_BYTE
export ESMF_NO_INTEGER_1_BYTE = default
endif

ifndef ESMF_NO_INTEGER_2_BYTE
export ESMF_NO_INTEGER_2_BYTE = default
endif

ifndef ESMF_FORTRANSYMBOLS
export ESMF_FORTRANSYMBOLS = default
endif

ifndef ESMF_TESTEXHAUSTIVE
export ESMF_TESTEXHAUSTIVE = default
endif

ifndef ESMF_TESTWITHTHREADS
export ESMF_TESTWITHTHREADS = default
endif

ifndef ESMF_TESTMPMD
export ESMF_TESTMPMD = default
endif

#-------------------------------------------------------------------------------
# For some variables having the literal string "default" is ok; 
# for others, look for this string and override it.
#-------------------------------------------------------------------------------

ifeq ($(ESMF_BUILD),default)
export ESMF_BUILD := $(ESMF_DIR)
endif

ifeq ($(ESMF_OS),default)
export ESMF_OS := $(shell $(ESMF_DIR)/scripts/esmf_os)
endif

ifeq ($(ESMF_OS),Linux)
# set ESMF_MACHINE for Linux
ifeq ($(ESMF_MACHINE),default)
export ESMF_MACHINE := $(shell uname -m)
endif
endif

ifeq ($(ESMF_OS),Cygwin)
# set ESMF_MACHINE for Cygwin
ifeq ($(ESMF_MACHINE),default)
export ESMF_MACHINE := $(shell uname -m)
endif
endif

ifeq ($(ESMF_OS),Darwin)
# set ESMF_MACHINE for Darwin
ifeq ($(ESMF_MACHINE),default)
export ESMF_MACHINE := $(shell uname -m)
# uname -m on Darwin (at least up to 8.11.1) is seriously broken and will
# always return i386 on any Intel system (it's hardcoded!)
ifeq ($(shell sysctl -n hw.optional.x86_64 2>&1),1)
export ESMF_MACHINE = x86_64
endif
endif
endif

ifeq ($(ESMF_ABI),default)
# start with 64-bit default for all architectures
export ESMF_ABI = 64

ifeq ($(ESMF_OS),Linux)
# default on Linux is 32-bit
export ESMF_ABI = 32
ifeq ($(ESMF_MACHINE),ia64)
# except for IA64
export ESMF_ABI = 64
endif
ifeq ($(ESMF_MACHINE),x86_64)
# and x86_64
export ESMF_ABI = 64
endif
endif

ifeq ($(ESMF_OS),Darwin)
# default on Darwin is 32-bit
export ESMF_ABI = 32
ifeq ($(ESMF_MACHINE),x86_64)
# except x86_64
export ESMF_ABI = 64
endif
endif

ifeq ($(ESMF_OS),Cygwin)
# default on Cygwin is 32-bit
export ESMF_ABI = 32
ifeq ($(ESMF_MACHINE),ia64)
# except for IA64
export ESMF_ABI = 64
endif
ifeq ($(ESMF_MACHINE),x86_64)
# and x86_64
export ESMF_ABI = 64
endif
endif

endif

# by default ABISTRING is simply ABI
ESMF_ABISTRING = $(ESMF_ABI)

ifeq ($(ESMF_COMPILER),default)
ifeq ($(ESMF_OS),Darwin)
export ESMF_COMPILER = absoft
ifeq ($(ESMF_MACHINE),i386)
export ESMF_COMPILER = intel
endif
ifeq ($(ESMF_MACHINE),x86_64)
export ESMF_COMPILER = intel
endif
endif
ifeq ($(ESMF_OS),Linux)
export ESMF_COMPILER = intel
endif
endif

ifeq ($(ESMF_BOPT),default)
export ESMF_BOPT = O
endif

ifneq ($(ESMF_ARRAY_LITE),TRUE)
export ESMF_ARRAY_LITE = FALSE
endif

ifneq ($(ESMF_NO_INTEGER_1_BYTE),TRUE)
export ESMF_NO_INTEGER_1_BYTE = FALSE
endif

ifneq ($(ESMF_NO_INTEGER_2_BYTE),TRUE)
export ESMF_NO_INTEGER_2_BYTE = FALSE
endif

ifneq ($(ESMF_TESTEXHAUSTIVE),ON)
export ESMF_TESTEXHAUSTIVE = OFF
endif

ifneq ($(ESMF_TESTWITHTHREADS),ON)
export ESMF_TESTWITHTHREADS = OFF
endif

ifneq ($(ESMF_TESTMPMD),ON)
export ESMF_TESTMPMD = OFF
endif

#-------------------------------------------------------------------------------
# If INSTALL environment variables are not set give them default values #-------------------------------------------------------------------------------

ifndef ESMF_INSTALL_PREFIX
ESMF_INSTALL_PREFIX := ./DEFAULTINSTALLDIR
endif
ESMF_INSTALL_PREFIX_ABSPATH := $(shell $(ESMF_DIR)/scripts/abspath $(ESMF_INSTALL_PREFIX))

ifndef ESMF_INSTALL_HEADERDIR
ESMF_INSTALL_HEADERDIR := include
endif
pathtype := $(shell $(ESMF_DIR)/scripts/pathtype $(ESMF_INSTALL_HEADERDIR))
ifeq ($(pathtype),rel)
export ESMF_INSTALL_HEADERDIR_ABSPATH = $(ESMF_INSTALL_PREFIX_ABSPATH)/$(ESMF_INSTALL_HEADERDIR)
else
export ESMF_INSTALL_HEADERDIR_ABSPATH = $(ESMF_INSTALL_HEADERDIR)
endif

ifndef ESMF_INSTALL_MODDIR
ESMF_INSTALL_MODDIR = mod/mod$(ESMF_BOPT)/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)
endif
pathtype := $(shell $(ESMF_DIR)/scripts/pathtype $(ESMF_INSTALL_MODDIR))
ifeq ($(pathtype),rel)
export ESMF_INSTALL_MODDIR_ABSPATH = $(ESMF_INSTALL_PREFIX_ABSPATH)/$(ESMF_INSTALL_MODDIR)
else
export ESMF_INSTALL_MODDIR_ABSPATH = $(ESMF_INSTALL_MODDIR)
endif

ifndef ESMF_INSTALL_LIBDIR
ESMF_INSTALL_LIBDIR = lib/lib$(ESMF_BOPT)/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)
endif
pathtype := $(shell $(ESMF_DIR)/scripts/pathtype $(ESMF_INSTALL_LIBDIR))
ifeq ($(pathtype),rel)
export ESMF_INSTALL_LIBDIR_ABSPATH = $(ESMF_INSTALL_PREFIX_ABSPATH)/$(ESMF_INSTALL_LIBDIR)
else
export ESMF_INSTALL_LIBDIR_ABSPATH = $(ESMF_INSTALL_LIBDIR)
endif

ifndef ESMF_INSTALL_DOCDIR
ESMF_INSTALL_DOCDIR := doc
endif
pathtype := $(shell $(ESMF_DIR)/scripts/pathtype $(ESMF_INSTALL_DOCDIR))
ifeq ($(pathtype),rel)
export ESMF_INSTALL_DOCDIR_ABSPATH = $(ESMF_INSTALL_PREFIX_ABSPATH)/$(ESMF_INSTALL_DOCDIR)
else
export ESMF_INSTALL_DOCDIR_ABSPATH = $(ESMF_INSTALL_DOCDIR)
endif

#-------------------------------------------------------------------------------
# TODO: in general ESMF_BUILD is respected - most generated files are created
# underneath ESMF_BUILD and not ESMF_DIR.  but there are exceptions.
# the ones i know about are:  
# - in the build_config/platform-specific directories are config header files, 
# so if these config files are moved, a -I flag will also have to be updated to 
# point to the new location.  the complication is that since these are per-platform files
# and since we promise to support building for multiple architectures from
# the same source tree, these files cannot go into a generic include dir.
# - the 'storeh:' target copies include files into src/include under the
# distribution tree.  
# - the system tests and demos (not sure about the unit tests and examples) 
# are compiled with the current dir set to the src dir (this is
# i think because if there are multiple .o files, it gets complicated to make
# them, get their names to link them, and then remove just them if you're 
# working in the test or examples dir - but still, it should be fixed.)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Keep this variable for now until rules are fixed to work without.
export ESMF_BATCHDEPRECATED = false
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Common variables
#-------------------------------------------------------------------------------

# same as ESMF_LIBDIR
ESMF_LDIR	= $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)

# library directory
ESMF_LIBDIR     = $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)

# f90 module directory
ESMF_MODDIR     = $(ESMF_BUILD)/mod/mod$(ESMF_BOPT)/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)

# test executable directory
ESMF_TESTDIR    = $(ESMF_BUILD)/test/test$(ESMF_BOPT)/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)

# example executable diretory
ESMF_EXDIR      = $(ESMF_BUILD)/examples/examples$(ESMF_BOPT)/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)

# include file directory
ESMF_INCDIR     = $(ESMF_BUILD)/src/include

# Infrastructure/Superstructure incs
ESMF_INTERNALINCDIRS  = -I$(ESMF_BUILD)/src/Infrastructure -I$(ESMF_BUILD)/src/Superstructure

# documentation directory
ESMF_DOCDIR	= $(ESMF_DIR)/doc

# ???
ESMF_BUILD_DOCDIR = $(ESMF_BUILD)/build/doc

# system test source directory
ESMF_STDIR      = $(ESMF_DIR)/src/system_tests

# platform specific configuration directory
ESMF_CONFDIR    = $(ESMF_DIR)/build_config/$(ESMF_OS).$(ESMF_COMPILER).default

# site specific configuration directory
ESMF_SITEDIR    = $(ESMF_DIR)/build_config/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_SITE)

# use test cases source directory
ESMF_UTCDIR     = $(ESMF_DIR)/src/use_test_cases

# use test cases scripts directory
ESMF_UTCSCRIPTS = $(ESMF_DIR)/src/use_test_cases/scripts

# documentation scripts and tools
ESMF_TEMPLATES	= $(ESMF_DIR)/scripts/doc_templates/templates
PROTEX		= $(ESMF_TEMPLATES)/protex 
CC_PROTEX       = $(ESMF_TEMPLATES)/scripts/do_ccprotex 
CH_PROTEX       = $(ESMF_TEMPLATES)/scripts/do_chprotex 
F_PROTEX        = $(ESMF_TEMPLATES)/scripts/do_fprotex 
DO_LATEX	= $(ESMF_TEMPLATES)/scripts/do_latex
DO_L2H		= $(ESMF_TEMPLATES)/scripts/do_l2h

# test script variables
UNIT_TESTS_CONFIG   = $(ESMF_TESTDIR)/unit_tests.config
SYS_TESTS_CONFIG    = $(ESMF_TESTDIR)/sys_tests.config
ESMF_TESTSCRIPTS    = $(ESMF_DIR)/scripts/test_scripts
DO_UT_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_ut_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -b $(ESMF_BOPT)
DO_EX_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_ex_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_EXDIR) -b $(ESMF_BOPT)
DO_ST_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_st_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -b $(ESMF_BOPT) 
DO_SUM_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_summary.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -e $(ESMF_EXDIR) -b $(ESMF_BOPT) 
DO_UTC_RESULTS	    = $(ESMF_UTCSCRIPTS)/do_utc_results.pl -h $(ESMF_UTCSCRIPTS) -d $(ESMF_TESTDIR) -b $(ESMF_BOPT) -e $(ESMF_MAX_PROCS)

# C specific variables
ESMC_OBJDIR	= $(ESMF_MODDIR)
ESMC_TESTDIR    = $(ESMF_TESTDIR)
ESMC_DOCDIR	= $(ESMF_DOCDIR)

#-------------------------------------------------------------------------------
# Add to Fortran preprocessing flags according to environment variables
ifeq ($(ESMF_ARRAY_LITE),TRUE)
FPPDEFS += -DESMF_NO_GREATER_THAN_4D
endif           
ifeq ($(ESMF_NO_INTEGER_1_BYTE),TRUE)
FPPDEFS += -DESMF_NO_INTEGER_1_BYTE
endif           
ifeq ($(ESMF_NO_INTEGER_2_BYTE),TRUE)
FPPDEFS += -DESMF_NO_INTEGER_2_BYTE
endif           
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# default settings for common.mk
# the ESMF_xxxDEFAULT values are only used if ESMF_xxx is not defined in
# user's environment.
#-------------------------------------------------------------------------------
ESMF_PTHREADSDEFAULT        = ON

ESMF_ARDEFAULT              = ar
ESMF_ARCREATEFLAGSDEFAULT   = cr
ESMF_AREXTRACTDEFAULT       = $(ESMF_ARDEFAULT) -x
ESMF_RANLIBDEFAULT          = ranlib
ESMF_SEDDEFAULT             = sed
ESMF_CPPDEFAULT             = gcc

ESMF_RM                     = rm -rf
ESMF_MV                     = mv -f
ESMF_WC                     = wc 
ESMF_GREPV                  = grep -v

ESMF_RPATHPREFIX     = -Wl,-rpath,

ESMF_F90OPTFLAG_X           =
ESMF_CXXOPTFLAG_X           =
ESMF_F90OPTFLAG_G           = -g
ESMF_CXXOPTFLAG_G           = -g

# setting default optimization flags is platform dependent
ifneq ($(origin ESMF_OPTLEVEL), environment)
ESMF_OPTLEVEL = $(ESMF_OPTLEVELDEFAULT)
endif
ifneq ($(ESMF_OPTLEVEL),default)
# if NEC, insert option before -O
ifeq ($(ESMF_COMPILER),sxcross)
ESMF_F90OPTFLAG_O =  -Wf" -O $(ESMF_OPTLEVEL)"
else
ESMF_F90OPTFLAG_O =  -O$(ESMF_OPTLEVEL)
endif
ESMF_CXXOPTFLAG_O = -O$(ESMF_OPTLEVEL) -DNDEBUG
else
# if NEC, insert option before -O
ifeq ($(ESMF_COMPILER),sxcross)
ESMF_F90OPTFLAG_O = -Wf -O
else
ESMF_F90OPTFLAG_O = -O
endif
ESMF_CXXOPTFLAG_O = -O -DNDEBUG
endif


#-------------------------------------------------------------------------------
# Set default ESMF_ variables which may be appended to or overridden in 
# platform specific build_rules.mk files.
#-------------------------------------------------------------------------------

# - F90COMPILER
ifneq ($(origin ESMF_F90COMPILER), environment)
ifeq ($(origin ESMF_F90), environment)
ESMF_F90COMPILER = $(ESMF_F90)
else
ESMF_F90COMPILER = $(ESMF_F90COMPILERDEFAULT)
ESMF_F90COMPILERDEFAULT = $(ESMF_F90DEFAULT)
endif
endif
ifneq ($(origin ESMF_F90IMOD), environment)
ESMF_F90IMOD = -I
endif
ifneq ($(origin ESMF_F90MODDIR), environment)
ESMF_F90MODDIR = $(ESMF_MODDIR)
endif
ifneq ($(origin ESMF_F90OPTFLAG), environment)
ESMF_F90OPTFLAG = $(ESMF_F90OPTFLAG_X)
ifeq ($(ESMF_BOPT),g)
ESMF_F90OPTFLAG = $(ESMF_F90OPTFLAG_G)
endif
ifeq ($(ESMF_BOPT),O)
ESMF_F90OPTFLAG = $(ESMF_F90OPTFLAG_O)
endif
endif
ESMF_F90COMPILEOPTS += $(ESMF_F90OPTFLAG)
ESMF_F90COMPILEPATHS += $(ESMF_F90IMOD)$(ESMF_F90MODDIR)
ESMF_F90COMPILEPATHSLOCAL =
ifneq ($(ESMF_SITE),default)
ESMF_F90COMPILEPATHSLOCAL += -I$(ESMF_SITEDIR)
endif
ESMF_F90COMPILEPATHSLOCAL += -I$(ESMF_CONFDIR) $(ESMF_INTERNALINCDIRS)
ESMF_F90COMPILEPATHS += -I$(ESMF_INCDIR)
ESMF_F90COMPILEFREECPP +=
ESMF_F90COMPILEFREENOCPP +=
ESMF_F90COMPILEFIXCPP +=
ESMF_F90COMPILEFIXNOCPP +=
ESMF_F90COMPILECPPFLAGS += $(FPPFLAGS)

# - CXXCOMPILER
ifneq ($(origin ESMF_CXXCOMPILER), environment)
ifeq ($(origin ESMF_CXX), environment)
ESMF_CXXCOMPILER = $(ESMF_CXX)
else
ESMF_CXXCOMPILER = $(ESMF_CXXCOMPILERDEFAULT)
ESMF_CXXCOMPILERDEFAULT = $(ESMF_CXXDEFAULT)
endif
endif
ifneq ($(origin ESMF_CXXOPTFLAG), environment)
ESMF_CXXOPTFLAG = $(ESMF_CXXOPTFLAG_X)
ifeq ($(ESMF_BOPT),g)
ESMF_CXXOPTFLAG = $(ESMF_CXXOPTFLAG_G)
endif
ifeq ($(ESMF_BOPT),O)
ESMF_CXXOPTFLAG = $(ESMF_CXXOPTFLAG_O)
endif
endif
ESMF_CXXCOMPILEOPTS += $(ESMF_CXXOPTFLAG)
ESMF_CXXCOMPILEPATHSLOCAL = -I$(ESMF_DIR)/$(LOCDIR)
ESMF_CXXCOMPILEPATHSLOCAL += -I$(ESMF_DIR)/$(LOCDIR)/../include 
ifneq ($(ESMF_SITE),default)
ESMF_CXXCOMPILEPATHSLOCAL += -I$(ESMF_SITEDIR)
endif
ESMF_CXXCOMPILEPATHSLOCAL += -I$(ESMF_CONFDIR) $(ESMF_INTERNALINCDIRS)
ESMF_CXXCOMPILEPATHS += -I$(ESMF_INCDIR)
ESMF_CXXCOMPILECPPFLAGS += $(CPPFLAGS) -D__SDIR__='"$(LOCDIR)"'

# - F90LINKER
ifneq ($(origin ESMF_F90LINKER), environment)
ifeq ($(origin ESMF_F90), environment)
ESMF_F90LINKER = $(ESMF_F90)
else
ESMF_F90LINKER = $(ESMF_F90LINKERDEFAULT)
ESMF_F90LINKERDEFAULT = $(ESMF_F90DEFAULT)
endif
endif
ESMF_F90LINKOPTS +=
ESMF_F90LINKPATHS += -L$(ESMF_LDIR)
ESMF_F90LINKRPATHS += $(ESMF_RPATHPREFIX)$(ESMF_LDIR)
ESMF_F90LINKLIBS +=
ESMF_F90ESMFLINKLIBS += -lesmf $(ESMF_F90LINKLIBS)

# - CXXLINKER
ifneq ($(origin ESMF_CXXLINKER), environment)
ifeq ($(origin ESMF_CXX), environment)
ESMF_CXXLINKER = $(ESMF_CXX)
else
ESMF_CXXLINKER = $(ESMF_CXXLINKERDEFAULT)
ESMF_CXXLINKERDEFAULT = $(ESMF_CXXDEFAULT)
endif
endif
ESMF_CXXLINKOPTS +=
ESMF_CXXLINKPATHS += -L$(ESMF_LDIR)
ESMF_CXXLINKRPATHS += $(ESMF_RPATHPREFIX)$(ESMF_LDIR)
ESMF_CXXLINKLIBS +=
ESMF_CXXESMFLINKLIBS += -lesmf $(ESMF_CXXLINKLIBS)

# - tools: AR + RANLIB + ...
ifneq ($(origin ESMF_AR), environment)
ESMF_AR = $(ESMF_ARDEFAULT)
endif
ifneq ($(origin ESMF_ARCREATEFLAGS), environment)
ESMF_ARCREATEFLAGS = $(ESMF_ARCREATEFLAGSDEFAULT)
endif
ifneq ($(origin ESMF_AREXTRACT), environment)
ESMF_AREXTRACT = $(ESMF_AREXTRACTDEFAULT)
endif
ifneq ($(origin ESMF_RANLIB), environment)
ESMF_RANLIB = $(ESMF_RANLIBDEFAULT)
endif
ifneq ($(origin ESMF_CPP), environment)
ESMF_CPP = $(ESMF_CPPDEFAULT)
endif
ifneq ($(origin ESMF_SED), environment)
ESMF_SED = $(ESMF_SEDDEFAULT)
endif

# - Shared library
ESMF_SL_SUFFIX        = so
ESMF_SL_LIBS_TO_MAKE  = libesmf
ESMF_SL_LIBLINKER     = $(ESMF_CXXCOMPILER)
ESMF_SL_LIBOPTS      +=
ESMF_SL_LIBLIBS      +=

# - MPIRUN
ifneq ($(origin ESMF_MPIRUN), environment)
ESMF_MPIRUN = $(ESMF_MPIRUNDEFAULT)
endif

# - MPIMPMDRUN
ifneq ($(origin ESMF_MPIMPMDRUN), environment)
ESMF_MPIMPMDRUN = $(ESMF_MPIMPMDRUNDEFAULT)
endif


#-------------------------------------------------------------------------------
# Up to here there have only been definitions, no targets.  This is the 
# first (and therefore default) target.  The definition of what "all" is
# should be defined in the top level makefile and not here.  If a different
# default is desired, that can also be defined in the top level makefile,
# before common.mk is included.
#-------------------------------------------------------------------------------

default: lib

#-------------------------------------------------------------------------------
#  Set up all defaults before here.   Next, include both the system dependent
#  makefile fragment, and if present, the site-specific makefile fragment.
#  These files can += to add on to existing defaults, or override settings
#  by setting flags and variables with = to new values.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#  Include the default platform-specific makefile fragment.
#-------------------------------------------------------------------------------

include $(ESMF_CONFDIR)/build_rules.mk


#-------------------------------------------------------------------------------
#  Include site specific makefile fragment.  If we want to suppress a warning
#  message if the site file is not found add a leading dash before the include
#  keyword.  (If the file is not found it is a warning, not a fatal error.)
#-------------------------------------------------------------------------------

ifneq ($(ESMF_SITE),default)
include $(ESMF_SITEDIR)/build_rules.mk
endif

#-------------------------------------------------------------------------------
#  Now all system-dependent files have been read.  Now anything
#  below here is again common code.  Variables should no longer be overwritten
#  with =, but should be appended to if neeeded with +=
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Set the correct MPIRUN command with appropriate options
#-------------------------------------------------------------------------------
ESMF_MPIRUNCOMMAND  = $(shell $(ESMF_DIR)/scripts/mpirun.command $(ESMF_DIR)/scripts $(ESMF_MPIRUN))
ifeq ($(ESMF_MPIRUNCOMMAND),esmfscript)
ESMF_MPIRUN := $(ESMF_DIR)/scripts/$(ESMF_MPIRUN) $(ESMF_MPISCRIPTOPTIONS)
endif

#-------------------------------------------------------------------------------
# For convenience ESMF_NETCDF_INCLUDE and ESMF_NETCDF_LIBPATH variables are 
# appended to the appropriate variables.
#-------------------------------------------------------------------------------
ifdef ESMF_NETCDF_INCLUDE
ESMF_CXXCOMPILEPATHS += -I$(ESMF_NETCDF_INCLUDE)
ESMF_CXXLINKPATHS    += -L$(ESMF_NETCDF_LIBPATH)
ESMF_CXXLINKLIBS     += -lnetcdf
ESMF_F90COMPILEPATHS += -I$(ESMF_NETCDF_INCLUDE)
ESMF_F90LINKPATHS    += -L$(ESMF_NETCDF_LIBPATH)
ESMF_F90LINKLIBS     += -lnetcdf
endif

#-------------------------------------------------------------------------------
# Override default pointer size macros if specified in the user environment
#-------------------------------------------------------------------------------
ifeq ($(origin ESMF_F90_PTR_BASE_SIZE), environment)
CPPFLAGS       += -DESMF_F90_PTR_BASE_SIZE=$(ESMF_F90_PTR_BASE_SIZE)
endif
ifeq ($(origin ESMF_F90_PTR_PLUS_RANK), environment)
CPPFLAGS       += -DESMF_F90_PTR_PLUS_RANK=$(ESMF_F90_PTR_PLUS_RANK)
endif

#-------------------------------------------------------------------------------
# ESMF_PTHREADS is passed (by CPP) into the library compilation to control the
# dependency on of the ESMF library on Pthreads.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_PTHREADS),OFF)
CPPFLAGS       += -DESMF_NO_PTHREADS
endif
# even when compiling with ESMF_PTHREADS=ON we need to find common header
ESMF_CXXCOMPILEPATHSLOCAL += -I$(ESMF_DIR)/src/Infrastructure/stubs/pthread

#-------------------------------------------------------------------------------
# ESMF_TESTEXHAUSTIVE is passed (by CPP) into test programs to control the
# number of tests that a test program will do.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_TESTEXHAUSTIVE),ON) 
CPPFLAGS       += -DESMF_TESTEXHAUSTIVE 
endif

#-------------------------------------------------------------------------------
# ESMF_TESTWITHTHREADS is passed (by CPP) into test programs to control the
# dependency on ESMF-threading.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_TESTWITHTHREADS),ON)
CPPFLAGS       += -DESMF_TESTWITHTHREADS
endif

#-------------------------------------------------------------------------------
# Add ESMF_ABISTRING to preprocessor flags
#-------------------------------------------------------------------------------

CPPFLAGS        +=-DS$(ESMF_ABISTRING)=1

#-------------------------------------------------------------------------------
# Add ESMF_OS to preprocessor flags
#-------------------------------------------------------------------------------

CPPFLAGS        +=-DESMF_OS_$(ESMF_OS)=1

#-------------------------------------------------------------------------------
# construct precompiler flags to be used on Fortran sources
#-------------------------------------------------------------------------------

FPPFLAGS        += $(addprefix $(ESMF_FPPPREFIX), $(FPPDEFS))
FPPFLAGS        += $(addprefix $(ESMF_FPPPREFIX), $(CPPFLAGS))


#-------------------------------------------------------------------------------
# common variables
LIBNAME		= $(ESMF_LIBDIR)/$(LIBBASE).a
ESMFLIB		= $(ESMF_LIBDIR)/libesmf.a
SOURCE		= $(SOURCEC) $(SOURCEF)
OBJS		= $(OBJSC) $(OBJSF)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# HOWTO:  Warning: Here there be dragons.
#
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
# 	cd $(ESMF_DIR) ;\
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
# and use ESMF_DIR for files which were checked out of CVS and only
# used as input.  ESMF_DIR is set to be ESMF_DIR for the framework
# and EVA builds, and to ESMF_IMPL_DIR for the Implementation Report.
# ESMF_BUILD always defaults to the same location as ESMF_DIR.
#
# Good luck.
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Create various directories where files expect to be copied into.
#-------------------------------------------------------------------------------
chk_dir: chkdir_lib

chkdir_lib:
	@if [ ! -d $(ESMF_LIBDIR) ]; then \
	  echo Making directory $(ESMF_LIBDIR) for library; \
	  mkdir -p $(ESMF_LIBDIR) ; fi
	@if [ ! -d $(ESMF_MODDIR) ]; then \
	  echo Making directory $(ESMF_MODDIR) for *.mod files; \
	  mkdir -p $(ESMF_MODDIR) ; fi

chkdir_doc:
	@if [ ! -d $(ESMF_DOCDIR) ]; then \
	  echo Making directory $(ESMF_DOCDIR) for documents; \
	  mkdir -p $(ESMF_DOCDIR) ; fi

chkdir_tests:
	@if [ ! -d $(ESMF_TESTDIR) ]; then \
	  echo Making directory $(ESMF_TESTDIR) for test output; \
	  mkdir -p $(ESMF_TESTDIR) ; fi

chkdir_include:
	@if [ ! -d $(ESMF_INCDIR) ]; then \
	  echo Making directory $(ESMF_INCDIR) for include files; \
	  mkdir -p $(ESMF_INCDIR) ; fi

chkdir_examples:
	@if [ ! -d $(ESMF_EXDIR) ]; then \
	  echo Making directory $(ESMF_EXDIR) for examples output; \
	  mkdir -p $(ESMF_EXDIR) ; fi


# use these targets if the libdir, testdir, etc. must be there already. 
# this target prints a fail message and exits if not present.
reqdir_lib:  
	@if [ ! -d $(ESMF_LIBDIR) ]; then \
	  echo "ESMF library directory not found:" ; \
	  echo " $(ESMF_LIBDIR) " ; \
	  echo "Library must be built first, or verify the current value of ESMF_BOPT" ; \
          echo " has the same setting as at library build time." ; \
	  echo "" ; \
          $(MAKE) err ; fi
	@if [ ! -d $(ESMF_MODDIR) ]; then \
	  echo "ESMF module directory not found:" ; \
	  echo " $(ESMF_MODDIR) " ; \
	  echo "Library must be built first, or verify the current value of ESMF_BOPT" ; \
          echo " has the same setting as at library build time." ; \
	  echo "" ; \
          $(MAKE) err ; fi

reqdir_tests:  
	@if [ ! -d $(ESMF_TESTDIR) ]; then \
	  echo "ESMF test directory not found:" ; \
	  echo " $(ESMF_TESTDIR) " ; \
	  echo "Tests must be built first, or verify the current value of ESMF_BOPT" ; \
          echo " has the same setting as at test build time." ; \
	  echo "" ; \
          $(MAKE) err ; fi

reqdir_examples:  
	@if [ ! -d $(ESMF_EXDIR) ]; then \
	  echo "ESMF examples directory not found:" ; \
	  echo " $(ESMF_EXDIR) " ; \
	  echo "Examples must be built first, or verify the current value of ESMF_BOPT" ; \
          echo " has the same setting as at example build time." ; \
	  echo "" ; \
          $(MAKE) err ; fi

#-------------------------------------------------------------------------------
# test to see if this will help our lack of real dependencies.  require that
# the file libesmf.a exists in the lib dir; if not, build it.  if it is there,
# call it success, even if a source file is more recent than the lib.

reqfile_libesmf:  
	@if [ ! -f $(ESMFLIB) ]; then \
	  $(MAKE) lib ; fi

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
VPATH = $(ESMF_DIR)/$(LOCDIR):$(ESMF_DIR)/include

libc:$(LIBNAME)($(OBJSC))
libf:$(LIBNAME)($(OBJSF))

# TODO: the dependencies need fixing here.
# the goal here is to only rebuild libesmf.a when a source file has
# changed - but this rule invokes a traversal of the entire source
# tree each time.   i guess what really needs to be done is that a
# real 'make depend' rule needs to make libesmf.a dependent on all
# the constituent .h, .C, and .F90 files without doing a full tree
# traversal.  having this line commented in makes it try to call the
# build_libs rule each time the unit tests, examples, or system tests
# are built, whether it's needed or not.
#
## building the libesmf.a file
#$(ESMFLIB):  build_libs


# Build all of ESMF from the top.  This target can be called from any
# subdir and it will go up to the top dir and build from there.
lib:  info build_libs info_mk

build_libs: chkdir_lib include
	cd $(ESMF_DIR) ;\
	$(MAKE) ACTION=tree_lib tree shared
	@echo "ESMF library built successfully."
	@echo "To verify, build and run the unit and system tests with: $(MAKE) check"
	@echo " or the more extensive: $(MAKE) all_tests"

# Build only stuff in and below the current dir.
build_here: chkdir_lib
	$(MAKE) ACTION=tree_lib tree shared

# Builds library - action for the 'tree' target.
tree_lib:
	dir=`pwd`; cd $(ESMF_MODDIR); $(MAKE) -f $${dir}/makefile MAKEFILE=$${dir}/makefile esmflib

# Builds library
esmflib:: chkdir_lib $(SOURCE)
	@if [ "$(SOURCEC)" != "" ] ; then \
	   $(MAKE) -f $(MAKEFILE) libc; fi
	@if [ "$(SOURCEF)" != "" ] ; then \
	    $(MAKE) -f $(MAKEFILE)  libf; fi
	@if [ "$(OBJS)" != "" -a "$(OBJS)" != " " ] ; then \
		$(ESMF_RANLIB) $(LIBNAME); \
		$(ESMF_RM) $(OBJS); \
	fi
	@if [ "$(QUICKSTART)" != "" ] ; then \
	   $(MAKE) -f $(MAKEFILE) tree_build_quick_start; fi


# copy private include files into src/include directory.
include: chkdir_include
	cd $(ESMF_DIR) ;\
	$(MAKE) ACTION=tree_include tree

# action for 'tree' target.
tree_include:
	@for hfile in ${STOREH} foo ; do \
	  if [ $$hfile != "foo" ]; then \
	    cp -f ../include/$$hfile $(ESMF_INCDIR) ; \
	  fi ; \
	done

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
CLEAN_DEFAULTS = *.o *.mod *.txt core ESM*.stdout ESM*.Log PET*.Log *ESMF_LogFile
CLEAN_TEXFILES = *.aux *.bbl *.blg *.log *.toc *.dvi *.ORIG

clean:
	$(MAKE) ACTION=tree_clean tree

# the GNU standard target is 'distclean' but we have had clobber in here
# for a long time, so for backward compatibility, leave them both.

distclean: clobber

# figure out if the current dir is the same as ESMF_DIR.  set the inode
# makefile variables here first, because it is easier to parse out the
# first word of the output with makefile builtins rather than depend on
# awk or some other command which must then be identical on every system 
# we try to support.   the -i on the ls command prints the numerical inode
# number of the directory; do it in this indirect way because the simple
# string comparison fails easily because of simple formatting differences
# (e.g. trailing slash vs not)

export INODE1 = $(word 1, $(shell ls -di .) )
export INODE2 = $(word 1, $(shell ls -di $(ESMF_DIR)) )

clobber:
	@if [ $(INODE1) != $(INODE2) ] ; then \
	  echo "Must run clobber from ESMF_DIR" ; \
	  echo "Current directory is `pwd`" ; \
	  echo "ESMF_DIR is $(ESMF_DIR)" ; \
	  echo "" ; \
	  $(MAKE) err ; \
	fi 
	$(MAKE) clean 
	@for DIR in $(CLOBBERDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      $(ESMF_RM) -r $$DIR ;\
	   fi ;\
	done


# action for 'tree' target.
tree_clean:
	@for DIR in $(CLEANDIRS) $(CLEAN_DEFDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      $(ESMF_RM) -r $$DIR ;\
	   fi ;\
	done
	$(ESMF_RM) $(CLEANFILES) $(CLEAN_DEFAULTS)
        
tree_cleanfiles:
	$(ESMF_RM) $(CLEANFILES) $(CLEAN_DEFAULTS)

# target which does a light cleaning - remove files only under the src dir 
#  (logfiles, doc files, test output files, files made by preprocessing, etc)
#  leaves the libs, executables, etc alone.
mostlyclean:
	@cd $(ESMF_BUILD)/src ;\
	$(MAKE) ACTION=tree_mostlyclean tree

tree_mostlyclean:
	@for DIR in $(DUSTDIRS) foo ; do \
	   if [ $$DIR != "foo" ] ; then \
	      cd $$DIR; $(MAKE) ACTION=tree_clean tree ;\
	   fi ;\
	done
	
#-------------------------------------------------------------------------------
# Generic target for building and running all tests, examples, and demos.
#-------------------------------------------------------------------------------

# vars used below in the all_tests target, because these are in the pattern
# (build, run), (build, run), ... not (build, build, ...) then (run, run, ...)

TEST_TARGETS = build_unit_tests run_unit_tests \
               build_system_tests run_system_tests

ALLTEST_TARGETS = $(TEST_TARGETS) \
                  build_examples run_examples \
                  build_demos run_demos

TEST_TARGETS_UNI = build_unit_tests run_unit_tests_uni \
                   build_system_tests run_system_tests_uni

ALLTEST_TARGETS_UNI = $(TEST_TARGETS_UNI) \
                      build_examples run_examples_uni \
                      build_demos run_demos_uni


# TODO: a bit more on what eventually these targets should be:
#
# according to the GNU conventions, 'gmake check' should test the build.
# so check builds and runs the unit and system tests with TESTEXHAUSTIVE
# pinned off.  this does a cursory check, not a full, exhaustive check.
#
# 'gmake all_tests' makes and runs the full set of tests, respecting the user
# setting for TESTEXHAUSTIVE.  it runs the unit tests, system tests, examples,
# and the demo.
#
# 'gmake validate' should probably do some numerical validation to make
# sure we have something like bit reproducibility, that we are not going to
# have wordsize problems, etc.   for now, we have no tests like that so it
# just runs the unit tests.
#

# quick sanity check, defaulting to TESTEXHAUSTIVE OFF but respecting
# the user setting if it already has a value.
check:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) info clean_check $(TEST_TARGETS_UNI) ;\
	else \
	  $(MAKE) info clean_check $(TEST_TARGETS) ;\
        fi


build_check:
	$(MAKE) build_unit_tests build_system_tests 


run_check:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) run_unit_tests_uni run_system_tests_uni ; \
	else \
	  $(MAKE) run_unit_tests run_system_tests ;\
        fi


clean_check:
	$(MAKE) clean_unit_tests clean_system_tests


# all tests, respecting user setting of TESTEXHAUSTIVE
all_tests: 
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) info $(ALLTEST_TARGETS_UNI) results_summary ;\
	else \
	  $(MAKE) info $(ALLTEST_TARGETS) results_summary ;\
        fi


build_all_tests: clean_if_exhaustive_flag_mismatch
	$(MAKE) build_unit_tests build_system_tests build_examples build_demos 


run_all_tests:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) run_unit_tests_uni run_system_tests_uni \
                  run_examples_uni run_demos_uni results_summary ;\
	else \
	  $(MAKE) run_unit_tests run_system_tests \
                  run_examples run_demos results_summary ;\
        fi

clean_all_tests:
	$(MAKE) clean_unit_tests clean_system_tests clean_examples clean_demos


# TODO: reserved for running any numerical validation tests, wordsize and
# precision tests - things which might give wrong computational answers.
# (currently just run the unit tests because these are not written yet.)
validate:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) unit_tests_uni ;\
	else \
	  $(MAKE) unit_tests ;\
        fi


build_validate:
	$(MAKE) build_unit_tests 


run_validate:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) run_unit_tests_uni ;\
	else \
	  $(MAKE) run_unit_tests ;\
        fi


clean_validate:
	$(MAKE) clean_unit_tests 



#-------------------------------------------------------------------------------
# Targets for building and running system tests.
#-------------------------------------------------------------------------------

system_tests: chkdir_tests
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR); fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
               echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
               exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi; \
	if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor system tests when ESMF_COMM is mpiuni;" ; \
	  echo "run system_tests_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
          exit ; \
	fi ; \
	$(MAKE) MULTI="Multiprocessor" config_sys_tests ; \
	$(MAKE) ACTION=tree_system_tests tree ; \
	$(MAKE) check_system_tests

tree_system_tests: tree_build_system_tests tree_run_system_tests

#
# system_tests_uni, build and run uni versions of the system tests
#
system_tests_uni: chkdir_tests
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR); fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
	fi ; \
	$(MAKE) MULTI="Uniprocessor" config_sys_tests
	$(MAKE) ACTION=tree_system_tests_uni tree ; \
	$(MAKE) check_system_tests

tree_system_tests_uni: tree_build_system_tests tree_run_system_tests_uni

#
# build_system_tests
#
build_system_tests: reqfile_libesmf reqdir_lib chkdir_tests 
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR) ; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi ; \
	$(MAKE) config_sys_tests
	$(MAKE) ACTION=tree_build_system_tests tree ; \
	echo "ESMF system tests built successfully."

tree_build_system_tests: $(SYSTEM_TESTS_BUILD) 


#
# TODO: the RM in the link rules below means that any system test which 
# includes additional .o files (which most do) will always rebuild even if 
# it is up-to-date.  but we remove the .o and .mod files because we also
# are required to be able to build multiple architectures from a single
# build tree.  we currently have a race-condition with the system tests
# in that we build with the current directory being the src dir, which
# means compilers can trample each others .o and .mod files.  the library
# cds into the lib or mod dir before compiling, so .o and .mod files
# are created in a compiler/platform directory and do not interfere with
# each other.  
# 
# the fix for this is either to cd into the test dir before compiling
# and linking, or to create a temp subdir based on the compiler/platform/
# BOPT/SITE settings - so compiles are truly independent.
#
# this also applies to the tests, examples, and demo code.
#

#
#  Link rule for Fortran system tests.
#
$(ESMF_TESTDIR)/ESMF_%STest : ESMF_%STest.o $(SYSTEM_TESTS_OBJ) $(ESMFLIB)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(SYSTEM_TESTS_OBJ) $< $(ESMF_F90ESMFLINKLIBS)
	$(ESMF_RM) -f *.o *.mod


# debugging aid:  link the executable, standard output, and log file to
# temporary names in the current directory (they are built in the test
# directory which is a long ways away from the source.  debuggers work
# better if the current directory is the source dir, not the executable dir.)
# example use:  gmake TNAME=FieldExcl system_test_links
system_test_links:
	$(ESMF_RM) t s l
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)STest t
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)STest.stdout s
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)STest.Log l

#
#  Link rule for Fortran system tests (MPMD).
#
$(ESMF_TESTDIR)/ESMF_%STestA : $(SYSTEM_TESTS_OBJ_A) $(ESMFLIB) ESMF_%STestA.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(SYSTEM_TESTS_OBJ_A) ESMF_$*STestA.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%STestB : $(SYSTEM_TESTS_OBJ_B) $(ESMFLIB) ESMF_%STestB.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(SYSTEM_TESTS_OBJ_B) ESMF_$*STestB.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%STestC : $(SYSTEM_TESTS_OBJ_C) $(ESMFLIB) ESMF_%STestC.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(SYSTEM_TESTS_OBJ_C) ESMF_$*STestC.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%STestD : $(SYSTEM_TESTS_OBJ_D) $(ESMFLIB) ESMF_%STestD.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(SYSTEM_TESTS_OBJ_D) ESMF_$*STestD.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%STestE : $(SYSTEM_TESTS_OBJ_E) $(ESMFLIB) ESMF_%STestE.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(SYSTEM_TESTS_OBJ_E) ESMF_$*STestE.o $(ESMF_F90ESMFLINKLIBS)
MPMDCLEANUP:
	$(ESMF_RM) -f *.o *.mod

#
# run_system_tests
#
run_system_tests:  reqdir_tests update_mpmd_flag
	@if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR) ; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi; \
	if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor system tests when ESMF_COMM is mpiuni;" ; \
	  echo "run run_system_tests_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
          exit; \
	fi; \
        if [ -f $(SYS_TESTS_CONFIG) ] ; then \
           $(ESMF_SED) -e 's/ [A-Za-z][A-Za-z]*processor/ Multiprocessor/' $(SYS_TESTS_CONFIG) > $(SYS_TESTS_CONFIG).temp; \
           $(ESMF_MV) $(SYS_TESTS_CONFIG).temp $(SYS_TESTS_CONFIG); \
        fi; \
	$(MAKE) ACTION=tree_run_system_tests tree ; \
	$(MAKE) check_system_tests

tree_run_system_tests: $(SYSTEM_TESTS_RUN) 

#
# run_system_tests_uni
#
run_system_tests_uni:  reqdir_tests update_mpmd_flag
	@if [ -f $(SYS_TESTS_CONFIG) ] ; then \
           $(ESMF_SED) -e 's/ [A-Za-z][A-Za-z]*processor/ Uniprocessor/' $(SYS_TESTS_CONFIG) > $(SYS_TESTS_CONFIG).temp; \
           $(ESMF_MV) $(SYS_TESTS_CONFIG).temp $(SYS_TESTS_CONFIG); \
        fi; \
	if [ -d $(ESMF_STDIR) ] ; then cd $(ESMF_STDIR) ; fi; \
	if [ ! $(SYSTEM_TEST)foo = foo ] ; then \
	   if [ -d $(SYSTEM_TEST) ] ; then \
	       cd $(SYSTEM_TEST); \
           else \
              echo "SYSTEM_TEST $(SYSTEM_TEST) does not exist."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi; \
	$(MAKE) ACTION=tree_run_system_tests_uni tree ; \
	$(MAKE) check_system_tests

tree_run_system_tests_uni: $(SYSTEM_TESTS_RUN_UNI)

#
# echo into a file how the tests were last built and run, so when the perl
# scripts run to check the results it can compute the number of messages that
# should be found.  it needs to know mpmd vs non-mpmd to know how many total
# tests we expected to execute; it needs to know multi vs uni so it knows
# how many messages per test are generated.
#
config_sys_tests:
	@echo "# This file used by test scripts, please do not delete." > $(SYS_TESTS_CONFIG)
ifeq ($(ESMF_TESTMPMD),ON)
ifeq ($(MULTI),)
	@echo "Last run Testmpmd ;  Noprocessor" >> $(SYS_TESTS_CONFIG)
else
	@echo "Last run Testmpmd ;" $(MULTI) >> $(SYS_TESTS_CONFIG)
endif
else
ifeq ($(MULTI),)
	@echo "Last run Nontestmpmd ; Noprocessor" >> $(SYS_TESTS_CONFIG)
else
	@echo "Last run Nontestmpmd ; " $(MULTI) >> $(SYS_TESTS_CONFIG)
endif
endif



#
# verify that either there is no SYS_TESTS_CONFIG file, or if one exists that
# the string testmpmd or Non-testmpmd matches the current setting of the
# ESMF_TESTMPMD environment variable.  
#
update_mpmd_flag:
ifeq ($(ESMF_TESTMPMD),ON)
	$(ESMF_SED) -e 's/ [A-Za-z][A-Za-z]*estmpmd/ Testmpmd/' $(SYS_TESTS_CONFIG) > $(SYS_TESTS_CONFIG).temp; \
	$(ESMF_MV) $(SYS_TESTS_CONFIG).temp $(SYS_TESTS_CONFIG);
else
	$(ESMF_SED) -e 's/ [A-Za-z][A-Za-z]*estmpmd/ Nontestmpmd/' $(SYS_TESTS_CONFIG) > $(SYS_TESTS_CONFIG).temp; \
	$(ESMF_MV) $(SYS_TESTS_CONFIG).temp $(SYS_TESTS_CONFIG);
endif

#
# run the systests, either redirecting the stdout from the command line, or
# relying on the mpirun script to redirect stdout from inside the batch script.
#
stest:
	-@if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)STest ; \
	  $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)STest ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)STest 1\> $(ESMF_TESTDIR)/ESMF_$(TNAME)STest.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)STest 1> $(ESMF_TESTDIR)/ESMF_$(TNAME)STest.stdout 2>&1 ; \
	fi 

#
# this target deletes only the system test related files from the test subdir
#
clean_system_tests:
	$(ESMF_RM) $(ESMF_TESTDIR)/*STest*  $(SYS_TESTS_CONFIG)
	$(ESMF_RM) $(ESMF_TESTDIR)/system_tests_results
	$(MAKE) ACTION=tree_cleanfiles tree

#
# report statistics on system tests
#
check_system_tests: 
	@$(DO_ST_RESULTS)


#-------------------------------------------------------------------------------
# Targets for building and running use test cases
#-------------------------------------------------------------------------------

use_test_cases: chkdir_tests
	@if [ -d $(ESMF_UTCDIR) ] ; then cd $(ESMF_UTCDIR); fi; \
	if [ ! $(USE_TEST_CASE)foo = foo ] ; then \
	   if [ -d $(USE_TEST_CASE) ] ; then \
	       cd $(USE_TEST_CASE); \
           else \
               echo "USE_TEST_CASE $(USE_TEST_CASE) does not exist."; \
               echo "Check out use_test_cases at the $(ESMF_DIR)/src directory."; \
               exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi; \
	if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor use test cases when ESMF_COMM is mpiuni;" ; \
	  echo "run use_test_cases_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
          exit ; \
	fi; \
	$(MAKE) ACTION=tree_use_test_cases tree ; \
	$(MAKE) check_use_test_cases

tree_use_test_cases: tree_build_use_test_cases tree_run_use_test_cases

#
# use_test_cases_uni, build and run uni versions of the use test cases
#
use_test_cases_uni: chkdir_tests
	@if [ -d $(ESMF_UTCDIR) ] ; then cd $(ESMF_UTCDIR); fi; \
	if [ ! $(USE_TEST_CASE)foo = foo ] ; then \
	   if [ -d $(USE_TEST_CASE) ] ; then \
	       cd $(USE_TEST_CASE); \
           else \
              echo "USE_TEST_CASE $(USE_TEST_CASE) does not exist."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
	fi; \
	$(MAKE) ACTION=tree_use_test_cases_uni tree ; \
	$(MAKE) check_use_test_cases

tree_use_test_cases_uni: tree_build_use_test_cases tree_run_use_test_cases_uni

#
# build_use_test_cases
#
build_use_test_cases: reqfile_libesmf reqdir_lib chkdir_tests
	@if [ -d $(ESMF_UTCDIR) ] ; then cd $(ESMF_UTCDIR) ; fi; \
	if [ ! $(USE_TEST_CASE)foo = foo ] ; then \
	   if [ -d $(USE_TEST_CASE) ] ; then \
	       cd $(USE_TEST_CASE); \
           else \
              echo "USE_TEST_CASE $(USE_TEST_CASE) does not exist."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi; \
	$(MAKE) ACTION=tree_build_use_test_cases tree ; \
	echo "ESMF use test cases built successfully."

tree_build_use_test_cases: chkdir_tests $(USE_TEST_CASES_BUILD)


#
# TODO: the RM in the link rules below means that any use test case which 
# includes additional .o files (which most do) will always rebuild even if 
# it is up-to-date.  but we remove the .o and .mod files because we also
# are required to be able to build multiple architectures from a single
# build tree.  we currently have a race-condition with the system tests
# in that we build with the current directory being the src dir, which
# means compilers can trample each others .o and .mod files.  the library
# cds into the lib or mod dir before compiling, so .o and .mod files
# are created in a compiler/platform directory and do not interfere with
# each other.  
# 
# the fix for this is either to cd into the test dir before compiling
# and linking, or to create a temp subdir based on the compiler/platform/
# BOPT/SITE settings - so compiles are truly independent.
#
# this also applies to the tests, examples, and demo code.
#

#
#  Link rule for Fortran use test cases.
#
$(ESMF_TESTDIR)/ESMF_%UseTestCase : ESMF_%UseTestCase.o $(USE_TEST_CASES_OBJ) $(ESMFLIB)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(USE_TEST_CASES_OBJ) $< $(ESMF_F90ESMFLINKLIBS)
	$(ESMF_RM) -f *.o *.mod


# debugging aid:  link the executable, standard output, and log file to
# temporary names in the current directory (they are built in the test
# directory which is a long ways away from the source.  debuggers work
# better if the current directory is the source dir, not the executable dir.)
# example use:  gmake TNAME=FieldExcl system_test_links
use_test_cases_links:
	$(ESMF_RM) t s l
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase t
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase.stdout s
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase.Log l

#
#  Link rule for Fortran use test cases (MPMD).
#
$(ESMF_TESTDIR)/ESMF_%UseTestCaseA : $(USE_TEST_CASES_OBJ_A) $(ESMFLIB) ESMF_%UseTestCaseA.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(USE_TEST_CASES_OBJ_A) ESMF_$*UseTestCaseA.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%UseTestCaseB : $(USE_TEST_CASES_OBJ_B) $(ESMFLIB) ESMF_%UseTestCaseB.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(USE_TEST_CASES_OBJ_B) ESMF_$*UseTestCaseB.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%UseTestCaseC : $(USE_TEST_CASES_OBJ_C) $(ESMFLIB) ESMF_%UseTestCaseC.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(USE_TEST_CASES_OBJ_C) ESMF_$*UseTestCaseC.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%UseTestCaseD : $(USE_TEST_CASES_OBJ_D) $(ESMFLIB) ESMF_%UseTestCaseD.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(USE_TEST_CASES_OBJ_D) ESMF_$*UseTestCaseD.o $(ESMF_F90ESMFLINKLIBS)
$(ESMF_TESTDIR)/ESMF_%UseTestCaseE : $(USE_TEST_CASES_OBJ_E) $(ESMFLIB) ESMF_%UseTestCaseE.o 
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(USE_TEST_CASES_OBJ_E) ESMF_$*UseTestCaseE.o $(ESMF_F90ESMFLINKLIBS)

#
# run_use_test_cases
#
run_use_test_cases:  reqdir_tests
	@if [ -d $(ESMF_UTCDIR) ] ; then cd $(ESMF_UTCDIR) ; fi; \
	if [ ! $(USE_TEST_CASE)foo = foo ] ; then \
	   if [ -d $(USE_TEST_CASE) ] ; then \
	       cd $(USE_TEST_CASE); \
           else \
              echo "USE_TEST_CASE $(USE_TEST_CASE) does not exist."; \
              echo "Check out use_test_cases at the $(ESMF_DIR)/src directory."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi; \
	if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor use test cases when ESMF_COMM is mpiuni;" ; \
	  echo "run run_use_test_cases_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
          exit ; \
	fi; \
	$(MAKE) ACTION=tree_run_use_test_cases tree ; \
	$(MAKE) check_use_test_cases

tree_run_use_test_cases: $(USE_TEST_CASES_RUN) 

#
# run_use_test_cases_uni
#
run_use_test_cases_uni:  reqdir_tests
	@if [ -d $(ESMF_UTCDIR) ] ; then cd $(ESMF_UTCDIR) ; fi; \
	if [ ! $(USE_TEST_CASE)foo = foo ] ; then \
	   if [ -d $(USE_TEST_CASE) ] ; then \
	       cd $(USE_TEST_CASE); \
           else \
              echo "USE_TEST_CASE  $(USE_TEST_CASE) does not exist."; \
               echo "Checkout use_test_cases at the $(ESMF_DIR)/src directory."; \
              exit; \
	   fi; \
	   echo current working directory is now `pwd` ; \
        fi; \
	$(MAKE) ACTION=tree_run_use_test_cases_uni tree ; \
	$(MAKE) check_use_test_cases

tree_run_use_test_cases_uni: $(USE_TEST_CASES_RUN_UNI)

#
# run the use test cases, either redirecting the stdout from the command line, or
# relying on the mpirun script to redirect stdout from inside the batch script.
#
uctest:
	-@if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase ; \
	  $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase 1\> $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase 1> $(ESMF_TESTDIR)/ESMF_$(TNAME)UseTestCase.stdout 2>&1 ; \
	fi 

#
# this target deletes only the use test cases related files from the test subdir
#
clean_use_test_cases:
	$(ESMF_RM) $(ESMF_TESTDIR)/*UseTestCase* 

#
# report statistics on system tests
#
check_use_test_cases: 
	@$(DO_UTC_RESULTS)


#-------------------------------------------------------------------------------
#  Targets for building and running unit tests.
#-------------------------------------------------------------------------------

# TODO: the run_unit_tests targets below a the dash before the make 
# subcommand ( -$(MAKE) xxx ) to ignore the return code from the command.
# i would prefer to not do this, but on at least one important platform (AIX) 
# we cannot force the fortran programs to exit with a zero return code if
# all is well (it comes out 128).  if this gets fixed in our code, the dashes
# can be removed and make can correctly stop on error.

unit_tests: chkdir_tests build_libs
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor unit tests when ESMF_COMM is mpiuni;" ; \
	  echo "run unit_tests_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
	fi
	$(MAKE) MULTI="Multiprocessor" config_unit_tests
	-$(MAKE) ACTION=tree_unit_tests tree
	$(MAKE) check_unit_tests

tree_unit_tests: tree_build_unit_tests tree_run_unit_tests

#
# tests_uni
#
unit_tests_uni: chkdir_tests build_libs
	$(MAKE) MULTI="Uniprocessor" config_unit_tests
	-$(MAKE) ACTION=tree_unit_tests_uni tree
	$(MAKE) check_unit_tests

tree_unit_tests_uni: tree_build_unit_tests tree_run_unit_tests_uni

#
# build_unit_tests
#
build_unit_tests: reqfile_libesmf reqdir_lib chkdir_tests verify_exhaustive_flag
	$(MAKE) config_unit_tests 
	$(MAKE) ACTION=tree_build_unit_tests tree
	@echo "ESMF unit tests built successfully."

tree_build_unit_tests: $(TESTS_BUILD)


$(ESMF_TESTDIR)/ESMF_%UTest : ESMF_%UTest.o $(ESMFLIB)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(UTEST_$(*)_OBJS) $< $(ESMF_F90ESMFLINKLIBS)
	$(ESMF_RM) -f *.o *.mod


$(ESMF_TESTDIR)/ESMC_%UTest : ESMC_%UTest.o $(ESMFLIB)
	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKRPATHS) -o $@ $(UTEST_$(*)_OBJS) $< $(ESMF_CXXESMFLINKLIBS)
	$(ESMF_RM) -f *.o *.mod

# debugging aid:  link the executable, standard output, and log file to
# temporary names in the current directory (they are built in the test
# directory which is a long ways away from the source.  debuggers work
# better if the current directory is the source dir, not the executable dir.)
# example use:  gmake TNAME=Field unit_test_links
unit_test_links:
	rm -f t s l
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)UTest t
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)UTest.stdout s
	ln -s $(ESMF_TESTDIR)/ESMF_$(TNAME)UTest.Log l

#
# run_unit_tests
#
run_unit_tests:  reqdir_tests verify_exhaustive_flag
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor unit tests when ESMF_COMM is mpiuni;" ; \
	  echo "run run_unit_tests_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
	fi 
	@if [ -f $(UNIT_TESTS_CONFIG) ] ; then \
	   $(ESMF_SED) -e 's/ [A-Za-z][A-Za-z]*processor/ Multiprocessor/' $(UNIT_TESTS_CONFIG) > $(UNIT_TESTS_CONFIG).temp; \
           $(ESMF_MV) $(UNIT_TESTS_CONFIG).temp $(UNIT_TESTS_CONFIG); \
        fi
	-$(MAKE) ACTION=tree_run_unit_tests tree
	$(MAKE) check_unit_tests

tree_run_unit_tests: $(TESTS_RUN) 

#
# run_unit_tests_uni
#
run_unit_tests_uni:  reqdir_tests verify_exhaustive_flag
	@if [ -f $(UNIT_TESTS_CONFIG) ] ; then \
	   $(ESMF_SED) -e 's/ [A-Za-z][A-Za-z]*processor/ Uniprocessor/' $(UNIT_TESTS_CONFIG) > $(UNIT_TESTS_CONFIG).temp; \
           $(ESMF_MV) $(UNIT_TESTS_CONFIG).temp $(UNIT_TESTS_CONFIG); \
        fi
	-$(MAKE) ACTION=tree_run_unit_tests_uni tree 
	$(MAKE) check_unit_tests

tree_run_unit_tests_uni: $(TESTS_RUN_UNI)

#
# echo into a file how the tests were last built and run, so when the perl
# scripts run to check the results it can compute the number of messages that
# should be found.  it needs to know exhaustive vs non to know how many total
# tests we expected to execute; it needs to know multi vs uni so it knows
# how many messages per test are generated.
#
config_unit_tests:
	@echo "# This file used by test scripts, please do not delete." > $(UNIT_TESTS_CONFIG)
ifeq ($(ESMF_TESTEXHAUSTIVE),ON) 
ifeq ($(MULTI),) 
	@echo "Last built Exhaustive ;  Last run Noprocessor" >> $(UNIT_TESTS_CONFIG)
else
	@echo "Last built Exhaustive ;  Last run" $(MULTI) >> $(UNIT_TESTS_CONFIG)
endif
else
ifeq ($(MULTI),) 
	@echo "Last built Non-exhaustive ;  Last run Noprocessor" >> $(UNIT_TESTS_CONFIG)
else
	@echo "Last built Non-exhaustive ;  Last run" $(MULTI) >> $(UNIT_TESTS_CONFIG)
endif
endif

#
# verify that either there is no UNIT_TESTS_CONFIG file, or if one exists that
# the string Exhaustive or Non-exhaustive matches the current setting of the
# ESMF_TESTEXHAUSTIVE environment variable.  this is used when trying to run
# already-built unit tests, to be sure the user has not changed the setting
# of exhaustive and then assumed that it will take effect.  unfortunately at
# this time, the flag is compile-time and not run-time.   
#
verify_exhaustive_flag:
ifeq ($(ESMF_TESTEXHAUSTIVE),ON) 
	@$(MAKE) UNIT_TEST_STRING="Exhaustive" exhaustive_flag_check
else
	@$(MAKE) UNIT_TEST_STRING="Non-exhaustive" exhaustive_flag_check
endif

exhaustive_flag_check:
	@if [ -s $(UNIT_TESTS_CONFIG) -a \
	     `$(ESMF_SED) -ne '/$(UNIT_TEST_STRING)/p' $(UNIT_TESTS_CONFIG) | $(ESMF_WC) -l` -ne 1 ] ; then \
	  echo "The ESMF_TESTEXHAUSTIVE environment variable is a compile-time control for" ;\
          echo "whether a basic set or an exhaustive set of tests are built." ;\
	  echo "" ;\
	  echo "The current setting of ESMF_TESTEXHAUSTIVE is \"$(ESMF_TESTEXHAUSTIVE)\", which" ;\
	  echo "is not the same as when the unit tests were last built." ;\
	  echo "(This is based on the contents of the file:" ;\
          echo "$(UNIT_TESTS_CONFIG) ";\
	  echo "which contains: `$(ESMF_SED) -e '1d' $(UNIT_TESTS_CONFIG)` )." ;\
	  echo "" ;\
	  echo "To rebuild and run the unit tests with the current ESMF_TESTEXHAUSTIVE value, run:" ;\
	  echo "   $(MAKE) clean_unit_tests unit_tests"  ;\
	  echo "or change ESMF_TESTEXHAUSTIVE to ON or OFF to match the build-time value." ;\
	  echo "" ;\
	  $(MAKE) err ;\
	fi

# call clean only if flags do not match
clean_if_exhaustive_flag_mismatch:
ifeq ($(ESMF_TESTEXHAUSTIVE),ON) 
	@$(MAKE) UNIT_TEST_STRING="Exhaustive" exhaustive_flag_clobber
else
	@$(MAKE) UNIT_TEST_STRING="Non-exhaustive" exhaustive_flag_clobber
endif

exhaustive_flag_clobber:
	@if [ -s $(UNIT_TESTS_CONFIG) -a \
	     `$(ESMF_SED) -ne '/$(UNIT_TEST_STRING)/p' $(UNIT_TESTS_CONFIG) | $(ESMF_WC) -l` -ne 1 ] ; then \
	  $(MAKE) clean_unit_tests ;\
	fi

#
# this target deletes only the unit test related files from the test subdir
# so we can rebuild them with the proper flags if that is what is needed.
#
clean_unit_tests:
	$(ESMF_RM) $(ESMF_TESTDIR)/*UTest* $(UNIT_TESTS_CONFIG)
	$(MAKE) ACTION=tree_cleanfiles tree


#
# report statistics on tests
#
check_unit_tests:
	@$(DO_UT_RESULTS)

#
# internal targets used to actually run the fortran and c++ unit tests
#
#  the call in the local makefiles is something like:
#    $(MAKE) TNAME=testname NP=4 ftest
#
# running a test is:  remove any old existing per-process log files, then
# run the test with the right number of processors.  the standard output is
# captured in a .stdout file; the test macros open PETx.name.Log files by
# default (set when the tests call ESMF_Initialize()).  after the tests run,
# we cat all the per-pet files together into a single log file.  (after the
# log can collate output from different PETs all by itself, we can remove
# the cat step.)
#
ftest:
	-@cd $(ESMF_TESTDIR) ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log ; \
	if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest 1\> ./ESMF_$(TNAME)UTest.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest 1> ./ESMF_$(TNAME)UTest.stdout 2>&1 ; \
	fi ; \
	cat ./PET*$(TNAME)UTest.Log > ./ESMF_$(TNAME)UTest.Log ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log

htest:
	-@cd $(ESMF_TESTDIR) ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log ; \
	if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest 1\> ./ESMF_$(HNAME)UTest.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest 1> ./ESMF_$(HNAME)UTest.stdout 2>&1 ; \
	fi ; \
	cat ./PET*$(TNAME)UTest.Log > ./ESMF_$(HNAME)UTest.Log ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log


ctest:
	-@cd $(ESMF_TESTDIR) ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log ; \
	if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(TNAME)UTest ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(TNAME)UTest ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(TNAME)UTest 1\> ./ESMC_$(TNAME)UTest.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(TNAME)UTest 1> ./ESMC_$(TNAME)UTest.stdout 2>&1 ; \
	fi ; \
	cat ./PET*$(TNAME)UTest.Log > ./ESMC_$(TNAME)UTest.Log ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log


#-------------------------------------------------------------------------------
#  Obsolete targets for building and running unit tests.  Echo an error
#  and point users to updated target names.
#-------------------------------------------------------------------------------

.PHONY: tests build_tests run_tests tests_uni run_tests_uni check_tests err

tests: ; $(error Obsolete target, use unit_tests now)

build_tests: ; $(error Obsolete target, use build_unit_tests now)

run_tests: ; $(error Obsolete target, use run_unit_tests now)

tests_uni: ; $(error Obsolete target, use unit_tests_uni now)

run_tests_uni: ; $(error Obsolete target, use run_unit_tests_uni now)

check_tests: ; $(error Obsolete target, use check_unit_tests now)

err: ; $(error gnumake exiting)


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
examples: chkdir_examples build_libs
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor examples when ESMF_COMM is mpiuni;" ; \
	  echo "run examples_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
	fi
	-$(MAKE) ACTION=tree_examples tree
	$(MAKE) check_examples


tree_examples: tree_build_examples tree_run_examples

#
# examples_uni
#
examples_uni: chkdir_examples build_libs
	-$(MAKE) ACTION=tree_examples_uni tree
	$(MAKE) check_examples

tree_examples_uni: tree_build_examples tree_run_examples_uni

#
# build_examples
#
build_examples: reqfile_libesmf reqdir_lib chkdir_examples
	$(MAKE) ACTION=tree_build_examples tree
	@echo "ESMF examples built successfully."

tree_build_examples: $(EXAMPLES_BUILD) 

#
#  Examples Link commands
#
$(ESMF_EXDIR)/ESMF_%Ex : ESMF_%Ex.o $(ESMFLIB)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(EXAMPLE_$(*)_OBJS) $< $(ESMF_F90ESMFLINKLIBS)
	$(ESMF_RM) -f *.o *.mod


$(ESMF_EXDIR)/ESMC_%Ex: ESMC_%Ex.o $(ESMFLIB)
	$(ESMF_CXXLINKER) $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKRPATHS) -o $@ $(EXAMPLE_$(*)_OBJS) $< $(ESMF_CXXESMFLINKLIBS)
	$(ESMF_RM) $<

#
# run_examples
#
run_examples:  reqdir_examples
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor examples when ESMF_COMM is mpiuni;" ; \
	  echo "run run_examples_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
	fi
	-$(MAKE) ACTION=tree_run_examples tree
	$(MAKE) check_examples

tree_run_examples: $(EXAMPLES_RUN) 


# run_examples_uni
#
run_examples_uni:  reqdir_examples
	-$(MAKE) ACTION=tree_run_examples_uni tree 
	$(MAKE) check_examples

tree_run_examples_uni: $(EXAMPLES_RUN_UNI)

#
# run the examples, either redirecting the stdout from the command line, or
# relying on the mpirun script to redirect stdout from inside the batch script.
#
exfrun:
	-@cd $(ESMF_EXDIR) ; \
	if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(EXNAME)Ex ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(EXNAME)Ex ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(EXNAME)Ex \> ./ESMF_$(EXNAME)Ex.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(EXNAME)Ex > ./ESMF_$(EXNAME)Ex.stdout 2>&1 ; \
	fi 

excrun:
	-@cd $(ESMF_EXDIR) ; \
	if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(EXNAME)Ex ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(EXNAME)Ex ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(EXNAME)Ex \> ./ESMC_$(EXNAME)Ex.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) ./ESMC_$(EXNAME)Ex > ./ESMC_$(EXNAME)Ex.stdout 2>&1 ; \
	fi 

#
# this target deletes only the example related files from the example subdir
#
clean_examples:
	$(ESMF_RM) $(ESMF_EXDIR)/*
	$(MAKE) ACTION=tree_cleanfiles tree

#
# report statistics on examples
#
check_examples:
	@$(DO_EX_RESULTS)


#-------------------------------------------------------------------------------
# Targets for building and running demos.
#-------------------------------------------------------------------------------

demos: build_libs chkdir_tests
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor demo when ESMF_COMM is mpiuni;" ; \
	  echo "run demos_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
	fi
	@if [ -d src/demos ] ; then cd src/demos; fi; \
	$(MAKE) ACTION=tree_demos tree

tree_demos: tree_build_demos tree_run_demos

demos_uni: build_libs chkdir_tests
	@if [ -d src/demos ] ; then cd src/demos; fi; \
	$(MAKE) ACTION=tree_demos_uni tree

tree_demos_uni: tree_build_demos tree_run_demos_uni

#
# build_demos
#
build_demos: reqfile_libesmf reqdir_lib chkdir_tests
	@if [ -d src/demos ] ; then cd src/demos; fi; \
	$(MAKE) ACTION=tree_build_demos tree
# TODO:FIELDINTEGRATION Restore once the demo is updated
#	@echo "ESMF demos built successfully."

tree_build_demos: $(DEMOS_BUILD) 

$(ESMF_TESTDIR)/%App : %Demo.o $(DEMOS_OBJ) $(ESMFLIB)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $(DEMOS_OBJ) $< $(ESMF_F90ESMFLINKLIBS)
	$(ESMF_RM) -f *.o *.mod


#
# run_demos
#
run_demos:  reqdir_tests
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
          echo "Cannot run multiprocessor demo when ESMF_COMM is mpiuni;" ; \
	  echo "run run_demos_uni instead." ; \
	  echo "" ; \
	  $(MAKE) err ; \
	fi
	@if [ -d src/demos ] ; then cd src/demos; fi; \
	$(MAKE) ACTION=tree_run_demos tree

tree_run_demos: $(DEMOS_RUN) 

run_demos_uni:  reqdir_tests
	@if [ -d src/demos ] ; then cd src/demos; fi; \
	$(MAKE) ACTION=tree_run_demos_uni tree

tree_run_demos_uni: $(DEMOS_RUN_UNI) 

#
# this target deletes only the demos and output files created by the demos
#
clean_demos:
	$(ESMF_RM) $(ESMF_TESTDIR)/*App 
	@if [ -d src/demos ] ; then cd src/demos; fi; \
	$(MAKE) clean
	


#-------------------------------------------------------------------------------
# Targets for checking the builds
#-------------------------------------------------------------------------------

check_results: check_unit_tests check_examples check_system_tests

results_summary:
	@$(DO_SUM_RESULTS)


#-------------------------------------------------------------------------------
# Targets for building the release, and the quick_start dirs
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------
# TODO: this section is frightfully out of date.   A release could be
# just the libs and mods, that plus quickstart, that plus the examples,
# that plus all the unit and system tests, that plus the demos.
#
# Rules for putting example files where they need to be for our
# binary releases (the pre-built libesmf.so and some simple examples).
# Creates the directory structure for releases, and copies example
# files and READMEs into it.
#
RELEASE_VERSION = 1_0_0rp2
RELEASE_SUBDIRS = example lib mod CoupledFlowSrc CoupledFlowExe
RELEASE_DIR     =  $(ESMF_BUILD)/release/esmf_$(RELEASE_VERSION)_$(ESMF_OS)_$(BOPT)_so

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
	      echo "Copying $$FILES.$(ESMF_OS) to $(RELEASE_DIR)/$(RELEASE_DESTDIR)/$$FILES" ;\
	      cp $$FILES.$(ESMF_OS) $(RELEASE_DIR)/$(RELEASE_DESTDIR)/$$FILES ;\
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
	@echo "========================================="
	@echo "doc rule from common.mk"
	@echo "=========================================" 
	@if [ ! -d $(ESMF_DIR)/src/doc ] ; then \
          echo "*** This version of the ESMF source tree does not contain documentation files. Please see http://www.esmf.ucar.edu/ for ESMF documentation." ; \
          $(MAKE) err; \
        fi;
	cd $(ESMF_DIR)/src/doc ;\
	$(MAKE) dvi html pdf
	@echo "Build doc completed."

# 'doc' and 'alldoc' do identical things now.
alldoc: doc

# this new target should be called from an individual
# subsystem doc directory and will build only that doc.
# this is also the default if you call make from a doc subdir.

onedoc: chkdir_doc include tex
	@echo "========================================="
	@echo "Building Single Document"
	@echo "========================================="
	@$(MAKE) dvi pdf html
	@echo "Build onedoc completed."

tex: chkdir_doc include
	cd $(ESMF_DIR) ;\
	$(MAKE) ACTION=tree_tex tree

tree_tex: $(TEXFILES_TO_MAKE)

dvi: chkdir_doc include tex
	@echo "========================================="
	@echo "dvi rule from common.mk, Building .dvi files"
	@echo "dvi files are:" $(DVIFILES)
	@echo "========================================="
	$(MAKE) $(DVIFILES)

tree_dvi: chkdir_doc $(DVIFILES)


pdf: chkdir_doc
	@echo "========================================="
	@echo "pdf rule from common.mk, Building .pdf files"
	@echo "pdf files are:" $(PDFFILES)
	@echo "========================================="
	$(MAKE) $(PDFFILES)

tree_pdf: chkdir_doc $(PDFFILES)


html: chkdir_doc include tex
	@echo "========================================="
	@echo "html rule from common.mk, Building .html files"
	@echo "html files are:" $(HTMLFILES)
	@echo "========================================="
	$(MAKE) $(HTMLFILES)

tree_html:chkdir_doc $(HTMLFILES)

clean_doc:
	@cd $(ESMF_BUILD)/src/doc ;\
	$(MAKE) tree_clean 

#-------------------------------------------------------------------------------
# Recursive calls
#-------------------------------------------------------------------------------

# TODO: old tree target explicitly exited if the return code was not 0 but
# this defeats the -k makeflag which ignores errors and continues as far
# as possible.
#tree: $(ACTION)
#	@if [ "$(DIRS)" != "" ]; then \
#	  for dir in $(DIRS) foo ; do \
#            if [ -d $$dir ]; then \
#              (cd $$dir ; \
#              echo $(ACTION) in: `pwd`; \
#              $(MAKE) -f makefile tree ACTION=$(ACTION));\
#              if [ "$$?" != 0 ]; then \
#                exit 1; \
#              fi; \
#            fi; \
#	  done; \
#        fi

# TODO: maybe this can be simpler somehow - but it seems to work this way.
# the findstring looks for the -k flag, which says to ignore errors.
# if present, then do not test for the return of make, and let it 
# continue as far as it can.  without -k, if there is an error in the
# call to make, exit from the tree command with a non-zero exit code
# so the calling make rule will exit.
tree: $(ACTION)
ifeq (,$(findstring k,$(MAKEFLAGS)))
	@if [ "$(DIRS)" != "" ]; then \
	  for dir in $(DIRS) foo ; do \
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
else
	@if [ "$(DIRS)" != "" ]; then \
	  for dir in $(DIRS) foo ; do \
            if [ -d $$dir ]; then \
              (cd $$dir ; \
              echo $(ACTION) in: `pwd`; \
              $(MAKE) -f makefile tree ACTION=$(ACTION));\
            fi; \
	  done; \
        fi
endif


#-------------------------------------------------------------------------------
# Suffixes
#-------------------------------------------------------------------------------
.SUFFIXES: .f .f90 .F .F90 $(SUFFIXES) .C .cc .r .rm .so .cppF90

#-------------------------------------------------------------------------------
#  Compile rules for F90, C++, and c files for both to .o and .a files
#-------------------------------------------------------------------------------

# TODO:  why were we not passing the mod dirpath to the .f and .f90 files?
# they are fixed format, but that does not mean they cannot use mods.
# i went ahead and added the mod dir to the rules but if this causes problems
# it should be removed.  it was not here originally and had been this way
# a long time.
# TODO more: add CXXFLAGS

.F90.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<

.f90.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

.F.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFIXCPP) $(ESMF_F90COMPILECPPFLAGS) $<

.f.o:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFIXNOCPP) $<

.c.o:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

.C.o:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

.F90.a:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(LIBNAME) $*.o
	$(ESMF_RM) $*.o

.f90.a:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(LIBNAME) $*.o
	$(ESMF_RM) $*.o

.F.a:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFIXCPP) $(ESMF_F90COMPILECPPFLAGS) $<
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(LIBNAME) $*.o
	$(ESMF_RM) $*.o

.f.a:
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHSLOCAL) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFIXNOCPP) $<
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(LIBNAME) $*.o
	$(ESMF_RM) $*.o

.c.a:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(LIBNAME) $*.o
	$(ESMF_RM) $*.o

.C.a:
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<
	@if [ "$(ESMF_DEP)" = "on" ] ; then \
           export ESMF_TMP=$(ESMF_LIBDIR) ; \
           makedepend -f- --  $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $< -- 2> /dev/null | \
	   perl -pe 's/(.*)\/([^\/]+):/$$ENV{'ESMF_TMP'}\/libesmf.a($$2):/' >> $(ESMF_DIR)/$(LOCDIR)/makefile.dep ; \
	 fi
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(LIBNAME) $*.o
	$(ESMF_RM) $*.o

# The rules below generate a valid Fortran file using gcc as a preprocessor:
# The -P option prevents putting #line directives in the output, and
# -E stops after preprocessing.
# The 'tr' command substitutes one-for-one, translating:
#   @ into newline to separate lines in multiline macros (the output
#     of the preprocessor is a single line which must be separated again)
#   ^ into # so that other preprocessor commands are ready to be processed by
#     the second runthrough of the preprocessor during the actual compile
#     (These lines are: ^include "fred.h" in the original source to shield 
#     them from the first preprocess pass.)
#   | into ' to be able to use single apostrophes in comments.
# The 'sed' command removes any lines which start #pragma GCC . These are
# generated by a couple versions of gcc and confuse the fortran compiler when
# trying to compile the newly generated file.
# The dir, notdir macros below are to be sure to create the .F90 file in the
# original source directory, since the makefile has already changed dirs into
# the mod dir to build.

ifeq ($(origin ESMF_CPPRULES),undefined)
.cpp.F90:
	$(ESMF_CPP) -E -P -I$(ESMF_INCDIR) $< | tr "@^" "\n#" | \
              $(ESMF_SED) -e '/^#pragma GCC/d' > $(dir $<)$(notdir $@)


.cppF90.F90:
	cp $< $<.cpp; $(ESMF_CPP) -E -P -I$(ESMF_INCDIR) $<.cpp | tr "@^|" "\n#'" | $(ESMF_SED) -e '/^#pragma GCC/d' > $(dir $<)$(notdir $@); rm -f $<.cpp


endif


#-------------------------------------------------------------------------------
#  Build shared library from regular lib (.so from .a)
#-------------------------------------------------------------------------------
shared:
	@if [ "$(ESMF_SL_LIBS_TO_MAKE)" != "" ] ; then \
		echo making shared libraries in $(ESMF_LDIR); \
		cd $(ESMF_LDIR) ; \
		$(ESMF_RM) -r tmp_* ; \
		for NEXTLIB in $(ESMF_SL_LIBS_TO_MAKE) foo ;\
		do \
		if [ -f $$NEXTLIB.a ] ; then \
		    $(ESMF_RM) $$NEXTLIB.$(ESMF_SL_SUFFIX) ; \
		    echo Converting $$NEXTLIB.a to $$NEXTLIB.$(ESMF_SL_SUFFIX) ;\
		    mkdir tmp_$$NEXTLIB ;\
		    cd tmp_$$NEXTLIB  ;\
	                $(ESMF_AREXTRACT) ../$$NEXTLIB.a ;\
                    echo $(ESMF_SL_LIBLINKER) $(ESMF_SL_LIBOPTS) -o $(ESMF_LDIR)/$$NEXTLIB.$(ESMF_SL_SUFFIX) *.o $(ESMF_SL_LIBLIBS) ;\
		    $(ESMF_SL_LIBLINKER) $(ESMF_SL_LIBOPTS) -o $(ESMF_LDIR)/$$NEXTLIB.$(ESMF_SL_SUFFIX) *.o $(ESMF_SL_LIBLIBS) ;\
		    cd .. ;\
		    $(ESMF_RM) -r tmp_$$NEXTLIB ;\
		fi ;\
		done ; \
	fi

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

%_chapi.tex : ../include/%.inc
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

TEXINPUTS_VALUE = ".:$(ESMF_DIR)/src/doc:$(ESMF_BUILD_DOCDIR):$(ESMF_DIR)/src/demos/coupled_flow:"
export TEXINPUTS_VALUE


#-------------------------------------------------------------------------------
#  dvi rules
#-------------------------------------------------------------------------------

%_refdoc.dvi : %_refdoc.ctex $(REFDOC_DEP_FILES)
	@echo "========================================="
	@echo "_refdoc.dvi rule from common.mk"
	@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* ref

#-------------------------------------------------------------------------------
#  pdf rules
#-------------------------------------------------------------------------------

$(ESMF_DOCDIR)/%.pdf: %.dvi
	@echo "========================================="
	@echo "_%pdf from %.dvi rule from common.mk"
	@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	dvipdf $< $@

#-------------------------------------------------------------------------------
#  html rules
#-------------------------------------------------------------------------------

$(ESMF_DOCDIR)/%_refdoc: %_refdoc.ctex $(REFDOC_DEP_FILES)
	@echo "========================================="
	@echo "_%refdoc from %.ctex rule from common.mk"
	@echo "========================================="
	@if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	$(DO_L2H) $* ref
	$(ESMF_RM) .latex2html-init
	$(ESMF_MV) $(@F) $(ESMF_DOCDIR)

#-------------------------------------------------------------------------------
#  These rules are for compiling the test examples.
#-------------------------------------------------------------------------------
.cpp.rm .cc.rm .C.rm .F.rm .f.rm .c.rm:
	-@$(ESMF_RM) $* *.o $*.mon.* gmon.out mon.out


#-------------------------------------------------------------------------------
# Keep .o files
#-------------------------------------------------------------------------------
.PRECIOUS: %.o
