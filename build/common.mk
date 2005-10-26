#  $Id: common.mk,v 1.130.2.2 2005/10/26 22:29:03 jwolfe Exp $
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

# name of directory containing the generated files from the build.
# defaults to the top dir, but can be set to be something different.
# TODO: in general this is respected - most generated files are created
# underneath ESMF_BUILD and not ESMF_TOP_DIR.  but there are exceptions.
# the ones i know about are:  
# - in the build_config/platform-specific directories is a config file 
# called 'machineinfo.h' which is generated at build time.  this 
# build_config dir is included in compiles, so if the machineinfo.h file
# (and conf.h) are moved, a -I flag will also have to be updated to point to
# the new location.  the complication is that since this a per-platform file
# and since we promise to support building for multiple architectures from
# the same source tree, these files cannot go into a generic include dir.
# - the 'storeh:' target copies include files into src/include under the
# distribution tree.  
# - the system tests and demos (not sure about the unit tests and examples) 
# are compiled with the current dir set to the src dir (this is
# i think because if there are multiple .o files, it gets complicated to make
# them, get their names to link them, and then remove just them if you're 
# working in the test or examples dir - but still, it should be fixed.)
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

# TODO: it is not clear to me that lahey should be the default Linux
# compiler.  intel or pgi seem more common these days.
ifeq ($(ESMF_COMPILER),default)

ifeq ($(ESMF_ARCH),Darwin)
export ESMF_COMPILER = absoft
endif

ifeq ($(ESMF_ARCH),Linux)
export ESMF_COMPILER = lahey
endif

endif


# If not set, the default for EXHAUSTIVE is OFF
ifndef ESMF_EXHAUSTIVE
export ESMF_EXHAUSTIVE = OFF
endif


# This is ok to remain as default
ifndef ESMF_SITE
export ESMF_SITE = default
endif


# Conditionally turn off ESMFs pthread feature set and use pthread_stubs
ifndef ESMF_PTHREADS
export ESMF_PTHREADS = ON
endif
ifneq ($(ESMF_PTHREADS),ON)
export ESMF_PTHREADS = OFF
endif


# If not set, the default for TESTWITHTHREADS is OFF
ifndef ESMF_TESTWITHTHREADS
export ESMF_TESTWITHTHREADS = OFF
endif



# if PREC is not already set, the default value is as follows:
#
# for non-Linux systems, the default is 64.  for those architectures which
# can only support 32-bit pointer size, this will be overridden in the
# compiler/platform dependent files. check the supported platform list for
# which pointer sizes are supported on which platform/compiler combinations. 
#
# for Linux systems, the default depends on what 'uname -m' reports back for
# the hardware type. 32-bit hardware (i686) defaults to 32; 64-bit 
# hardware (ia64) defaults to 64.  
#
# set ESMF_PREC explicitly if the default value is not what is wanted and the 
# makefiles will honor that if possible.  note that only a few platforms
# fully support code with more than one pointer size (e.g. ibm, irix)
#
# TODO: there is an inconsistency here.  the original requirement was the
# users wanted control over the default data word size (e.g. in fortran
# declaring a 'real' might be 4 bytes or 8, with some compilers having a
# compile-time flag to decide).   our use of the ESMF_PREC variable is only
# to control the code pointer size - e.g. whether a pointer is 4 or 8 bytes.
# this is a completely separate issue - one which must be dealt with, but
# we often mix the terms without being clear on what we are setting and what
# we are defaulting.  we do not let users control the default data item size
# with any sort of ESMF_xxx environment variable.  if they want to, they can
# add something to the fortran compile flags or C compile flags.  this should
# be cleaned up and clearly documented.

ifndef ESMF_PREC
ESMF_PREC = default
endif
ifeq ($(ESMF_PREC),default)
 ifeq ($(ESMF_ARCH),Linux)
  MACH_ARCH = $(shell uname -m)
  ifeq ($(MACH_ARCH),ia64)
   export ESMF_PREC = 64
  else
   export ESMF_PREC = 32
  endif
 else
  export ESMF_PREC = 64
 endif
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
MPI_INCLUDE = -I$(MPI_HOME)/include
MPI_LIB     = -L$(MPI_HOME)/lib
MPIRUN      = $(MPI_HOME)/bin/mpirun
endif

# if using PBS system, export this for run time
ifdef PBS_NODEFILE
export ESMF_NODES := -machinefile $(PBS_NODEFILE)
endif           

# compiler option:  g to add debugging info, O (oh) for optimized code.
# default compiler flag is optimized.  if you want to disable the optimization
# set ESMF_BOPT to O and set ESMF_OPTLEVEL to 0 (zero).  that will compile
# with -O0 (dash oh zero), which will disable all optimizations.
# (notice that the BOPT setting is simply the letter without a leading dash.)
ifndef ESMF_BOPT
export ESMF_BOPT = O
endif
ifeq ($(ESMF_BOPT),default)
export ESMF_BOPT = O
endif


# common commands and flags.  override in the platform specific include
# files if they differ.
AR		   = ar
AR_FLAGS	   = cr
AR_EXTRACT         = -x
AR32_64            = $(AR)
RM		   = rm -f
MV 		   = mv -f
RANLIB		   = ranlib
M4	           = m4
SED		   = sed 
WC		   = wc 

OMAKE		   = $(MAKE)
SHELL		   = /bin/sh

C_FC_MOD           = -I
C_CLINKER          = $(C_CXX)
C_FLINKER          = $(C_FC)
C_LINKOPTS         = 
C_SLFLAG           = -Wl,-rpath,

# TODO: make sure this has actually been fixed correctly.  the current problem
# is that on some platforms there are inappropriate dirs in the default
# LD_LIBRARY_PATH variable (e.g. IRIX and o32 vs n32 vs 64).
# so we can't just add these to all platforms.  on the other hand, for
# some archs this is the right approach.   my next attempt at fixing this
# will be:  make sure that any C_LIB_PATHS and C_LD_PATHS which were added
# in the platform dep files are added the LIB_PATHS and LD_PATHS vars, and
# those are added to the LINKFLAGS var, which is used at compile and link time.
# second, the environment is added to a separate variable, which can be
# appended to the C_LIB_PATHS and C_LD_PATHS variables in the platform-dep
# files.   i will see how far this one gets me.
# 
# append each directory which is in LD_LIBRARY_PATH to the -L flag and also 
# to the run-time load flag.  (on systems which support the 'module' command, 
# that is how it works - by adding dirs to LD_LIBRARY_PATH.)  if your libs 
# are not found, set LD_LIBRARY_PATH, or make a site specific file and edit 
# the paths explicitly.
ifeq ($(origin LD_LIBRARY_PATH), environment)
ENV_LIB_PATHS  = $(addprefix -L, $(subst :, ,$(LD_LIBRARY_PATH)))
ENV_LD_PATHS   = $(addprefix $(SLFLAG), $(subst :, ,$(LD_LIBRARY_PATH)))
endif

# SL in the next section refers to a shared library.  if a platform
# does not support building a shared library, redefine SL_LIBS_TO_MAKE
# to be empty in the platform-dependent makefile.
SL_SUFFIX          = so
SL_LIBS_TO_MAKE    = libesmf

# these flags are used during the building of the shared lib from the
# static lib.  which linker to use, which options the linker needs to
# build a shared lib, and which other libs (if any) need to be specified
# on the link line to satisfy undefined externals.
C_SL_LIBLINKER     = $(C_CXX)
C_SL_LIBOPTS       = 
C_SL_LIBLIBS       = 


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
#                    the file build/$(ESMF_ARCH)/base
# ESMF_BUILD       - Root directory to build in.  Set this variable on the make
#                    line to build somewhere other than ESMF_DIR.
# (these seem to be unused so far - and the right way to do installs seems
# like we should simply define a single ESMF_INSTALL_DIR target and have
# the makefile make include, mod, and lib dirs below that.)
# ESMF_LIB_INSTALL - Directory for install target to place libs.
# ESMF_MOD_INSTALL - Directory for install target to place mod files.
#-------------------------------------------------------------------------------

LDIR		= $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)

ESMF_LIBDIR     = $(ESMF_BUILD)/lib/lib$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_MODDIR     = $(ESMF_BUILD)/mod/mod$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_TESTDIR    = $(ESMF_BUILD)/test/test$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_EXDIR      = $(ESMF_BUILD)/examples/examples$(ESMF_BOPT)/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_PREC).$(ESMF_SITE)
ESMF_INCDIR     = $(ESMF_BUILD)/src/include
ESMF_DOCDIR	= $(ESMF_TOP_DIR)/doc
ESMF_BUILD_DOCDIR = $(ESMF_BUILD)/build/doc
ESMF_STDIR      = $(ESMF_TOP_DIR)/src/system_tests
ESMF_CONFDIR    = $(ESMF_TOP_DIR)/build_config/$(ESMF_ARCH).$(ESMF_COMPILER).default
ESMF_SITEDIR    = $(ESMF_TOP_DIR)/build_config/$(ESMF_ARCH).$(ESMF_COMPILER).$(ESMF_SITE)

# TODO: these may be leftovers from the impl_rep, and if so, they should
# be moved up into that makefile.  as far as i know, these are not used
# in the eva nor esmf framework builds.
# (why are there both ESMC_ and ESMF_ files here??)
ESMC_OBJDIR	= $(ESMF_MODDIR)
ESMC_TESTDIR    = $(ESMF_TESTDIR)
ESMC_DOCDIR	= $(ESMF_DOCDIR)

ESMF_TEMPLATES	= $(ESMF_TOP_DIR)/scripts/doc_templates/templates
PROTEX		= $(ESMF_TEMPLATES)/protex 
CC_PROTEX       = $(ESMF_TEMPLATES)/scripts/do_ccprotex 
CH_PROTEX       = $(ESMF_TEMPLATES)/scripts/do_chprotex 
F_PROTEX        = $(ESMF_TEMPLATES)/scripts/do_fprotex 
DO_LATEX	= $(ESMF_TEMPLATES)/scripts/do_latex
DO_L2H		= $(ESMF_TEMPLATES)/scripts/do_l2h

TESTS_CONFIG    = $(ESMF_TESTDIR)/tests.config
ESMF_TESTSCRIPTS    = $(ESMF_TOP_DIR)/scripts/test_scripts
DO_UT_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_ut_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -b $(ESMF_BOPT)
DO_EX_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_ex_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_EXDIR) -b $(ESMF_BOPT)
DO_ST_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_st_results.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -b $(ESMF_BOPT)
DO_SUM_RESULTS	    = $(ESMF_TESTSCRIPTS)/do_summary.pl -h $(ESMF_TESTSCRIPTS) -d $(ESMF_TESTDIR) -e $(ESMF_EXDIR) -b $(ESMF_BOPT)

# set up the defaults for all compilers, all options.  if the platform
# dependent files want to add flags, they can += more flags.  if they want
# to override these flags, they can simply reassign them.

# debug option
G_CFLAGS = -g
G_FFLAGS = -g

# optimize option - adjust if ESMF_OPTLEVEL is set
ifdef ESMF_OPTLEVEL
O_CFLAGS = -O$(ESMF_OPTLEVEL)
O_FFLAGS = -O$(ESMF_OPTLEVEL)
else
O_CFLAGS = -O
O_FFLAGS = -O
endif

#-------------------------------------------------------------------------------
# Up to here there have only been definitions, no targets.  This is the 
# first (and therefore default) target.  The definition of what "all" is
# differs for the framework, for the implementation report, and for the
# eva codes, so "all:" should be defined in the top level makefile and
# not here.  If a different default is desired, that can also be
# defined in the top level makefile, before common.mk is included.
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

MPI_HOME       = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
MPI_INCLUDE    = -I$(MPI_HOME)
MPI_LIB        = -lmpiuni
MPIRUN         = $(MPI_HOME)/mpirun
endif

#-------------------------------------------------------------------------------


# TODO: one possible alternate strategy:
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

# if ESMF_NO_IOCODE is set to true, then there is no reference to any
# netcdf calls from the esmf library.  Otherwise, there are references
# to netcdf routines, so if the user does not have them, we supply a
# stub library which simply prints an error if they are called. 

# The FPP line is commented out because i am experimenting with passing
# all CPP symbols to FPP so we do not have to set them both places.

ifeq ($(ESMF_NO_IOCODE),true)
# FPPFLAGS       += $(FPP_PREFIX)-DESMF_NO_IOCODE
CPPFLAGS       += -DESMF_NO_IOCODE
else
ifndef NETCDF_LIB
NETCDF_INCLUDE   = -I$(ESMF_DIR)/src/Infrastructure/stubs/netcdf_stubs
NETCDF_LIB       = -lnetcdf_stubs
endif
# at this point netcdf always has a value - either from the user pointing
# to the real lib location, or our stub lib.
EXTRA_INCLUDES += $(NETCDF_INCLUDE)
EXTRA_LIBS += $(NETCDF_LIB)
endif


ifeq ($(ESMF_PTHREADS),OFF)
CPPFLAGS       += -DESMF_NO_PTHREADS
endif
# this is needed even if compiling with pthreads on
EXTRA_INCLUDES += -I$(ESMF_DIR)/src/Infrastructure/stubs/pthread


ifeq ($(ESMF_TESTWITHTHREADS),ON)
CPPFLAGS       += -DESMF_TESTWITHTHREADS
endif


# TODO:  does this actually get used?   seems not.  leave it here until
# we are sure it is not needed, and then remove it.    (or put in cases
# for the external lib list below into the "EXTRA_LIBS" list above.)
#-------------------------------------------------------------------------------
# Defines all libraries needed for using linear and nonlinear solvers.
# The order of listing these libraries is important!
#
# PCONF - indicates which OPTIONAL external packages are available at your site
#-------------------------------------------------------------------------------
#
#PCONF		= $(ESMC_HAVE_MPE)  $(ESMC_HAVE_PARMETIS) \
#                  $(ESMC_HAVE_AMS)  $(ESMC_HAVE_X11)   $(ESMC_HAVE_MATLAB) \
#                  $(ESMC_HAVE_ADIC) $(ESMC_HAVE_JAVA)
#EXTERNAL_LIB	= $(MPE_LIB)        $(BLOCKSOLVE_LIB)  $(PARMETIS_LIB) \
#                  $(AMS_LIB)        $(SPAI_LIB) \
#                  $(ADIC_LIB) 
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#

LIBNAME		= $(ESMF_LIBDIR)/$(LIBBASE).a
ESMFLIB		= $(ESMF_LIBDIR)/libesmf.a

SOURCE		= $(SOURCEC) $(SOURCEF)
OBJS		= $(OBJSC) $(OBJSF)

# these flags vary because each sub-makefile resets LOCDIR to the local dir.
# i separated them so we could echo the non-local-dir flags for the info:
# target - since info runs from the top, it is misleading to indicate that
# those dirs are part of the search path.
ESMF_INCLUDE1	= -I$(ESMF_TOP_DIR)/$(LOCDIR) \
	 	  -I$(ESMF_TOP_DIR)/$(LOCDIR)/../include 

# if the SITE is set to something other than the default, search it
# first for include files.  this gives the option to replace the 
# configure files if needed, and also pick up the machineinfo.h file
# from the correct place.
ifneq ($(ESMF_SITE),default)
ESMF_INCLUDE2    = -I$(ESMF_SITEDIR)  \
		  -I$(ESMF_CONFDIR) 
else
ESMF_INCLUDE2    = -I$(ESMF_CONFDIR) 
endif

ESMF_INCLUDE2	+= $(LOCAL_INCLUDE) \
		   -I$(ESMF_INCDIR) -I$(ESMF_MODDIR) $(MPI_INCLUDE) \
                   $(EXTRA_INCLUDES)

# combine the relative-to-local-dir flags with the global dir flags.
ESMF_INCLUDE    = $(ESMF_INCLUDE1) $(ESMF_INCLUDE2)

ESMC_INCLUDE    = $(ESMF_INCLUDE)
CCPPFLAGS	+= $(PCONF) $(ESMC_PARCH) $(CPPFLAGS) -D__SDIR__='"$(LOCDIR)"'
FCPPFLAGS	+= $(PCONF) $(ESMC_PARCH) $(FPPFLAGS) 

# TODO:  these seem unused.  when sure, remove.
#C_SH_LIB_PATH	= $(SLFLAG)$(LDIR) $(C_DYLIBPATH)
#F_SH_LIB_PATH	= $(SLFLAG)$(LDIR) $(F_DYLIBPATH)

ESMC_TIME_LIB	 = -L$(LDIR)

#-------------------------------------------------------------------------------
#  Common variable definitions.
#-------------------------------------------------------------------------------

CC             = $(C_CC)
CXX	       = $(C_CXX)
FC	       = $(C_FC)

FC_MOD         = $(C_FC_MOD)

# unless CPP has already been defined in the platform-specific makefile,
# set it here.  on some platforms we might get away with calling the C 
# compiler to preprocess our fortran files, but on most it does not 
# produce code which is still valid fortran (e.g. breaking tokens up 
# according to C syntax rules).
# we cannot use the standard fpp fortran preprocessor because we require 
# the "##" preprocessor command, which transforms:  foo ## bar
# into the single token "foobar" (this is needed, for example, to generate
# unique function names via macro for each type/kind/rank of fortran array 
# arguments).  most standard fpp fortran preprocessors do not implement
# this function.  gcc does the proper thing on all platforms except X1,
# which is why we prefer to use it.  however, we realize that some systems
# do not install the gnu tools by default.  if this is a problem for you,
# you can try defining CPP in a site-specific makefile (try setting it to
# the C compiler) and see if you get lucky.
ifneq ($(origin CPP), file)
CPP	       = gcc
endif

#-------------------------------------------------------------------------------
# add in any FPPOPTS defined in the system dep files, and add a definition
#  for the selected word size (32/64) by defining the syms S32 or S64.
#-------------------------------------------------------------------------------
CPPFLAGS        +=-DS$(ESMF_PREC)=1 

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
F90CXXLIBS     = $(C_F90CXXLIBS)
CXXF90LIBS     = $(C_CXXF90LIBS)
# TODO: unused?  remove if so.
#CXXSO          = $(C_CXXSO)
#CXXSOLIBS      = $(C_CXXSOLIBS)
ESMC_LANGUAGE = CONLY
ESMC_SCALAR   = real

#-------------------------------------------------------------------------------
# Variable definitions for debug option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),g)
COPTFLAGS      = $(G_CFLAGS)
FOPTFLAGS      = $(G_FFLAGS)
endif

#-------------------------------------------------------------------------------
# Variable definitions for non-debug, non-optimized option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),)
COPTFLAGS      = $(X_CFLAGS)
FOPTFLAGS      = $(X_FFLAGS)
endif

#-------------------------------------------------------------------------------
# Variable definitions for optimized option.
#-------------------------------------------------------------------------------
ifeq ($(ESMF_BOPT),O)
COPTFLAGS      = $(O_CFLAGS)
FOPTFLAGS      = $(O_FFLAGS)
endif


#-------------------------------------------------------------------------------
# Now set the loader and linkers to use the selected set of flags.
#-------------------------------------------------------------------------------
# only used by impl report, as far as i can tell.  TODO: remove this
#CXXF90LD       = $(C_CXXF90LD) -L$(ESMF_LIBDIR)
#F90CXXLD       = $(C_F90CXXLD) -L$(ESMF_LIBDIR)

# building a shared lib.so from a lib.a
SL_LIBLINKER = $(C_SL_LIBLINKER)
SL_LIBOPTS   = $(C_SL_LIBOPTS)
SL_LIBLIBS   = $(C_SL_LIBLIBS)

# linking executables, taking into account libesmf might be shared
SLFLAG       = $(C_SLFLAG)

CLINKER      = $(C_CLINKER)
FLINKER      = $(C_FLINKER)
LINKOPTS     = $(C_LINKOPTS)

# TODO: these seem unused.  might be used by eva codes - if so, move
# these lines into the eva makefile.  unneeded by impl report or framework.
# original lines - do these options need to be here?
#FLINKER       = $(C_FLINKER) $(FOPTFLAGS) $(F_SH_LIB_PATH)
#CLINKER       = $(C_CLINKER) $(COPTFLAGS) $(C_SH_LIB_PATH)

# default is to include the esmf lib dir in the library search path
# plus anything set in the platform dep file.   if there is a load-time 
# flag needed for a platform, set C_LD_PATHS in the platform dep files and
# be sure to include $(LDIR).
LIB_PATHS = -L$(LDIR) $(C_LIB_PATHS)
ifneq ($(C_LD_PATHS),)
LD_PATHS  = $(C_LD_PATHS)
else
LD_PATHS  =
endif

# add the LIB_PATHS and LD_PATHS to the LINKOPTS - this does not automatically
# add the ENV parts - add them yourself in the platform-dep file if needed.
LINKOPTS  += $(LIB_PATHS) $(LD_PATHS)

# collect all the libs together in a single variable
CLINKLIBS = -lesmf $(MPI_LIB) $(EXTRA_LIBS) $(CXXF90LIBS)
FLINKLIBS = -lesmf $(MPI_LIB) $(EXTRA_LIBS) $(F90CXXLIBS)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# alias section:
# these are here to help the link fragment files.  if users include 
# common.mk file, then they can use these to link with.  make these 
# variables have ESMF_ since they are seen by the user.  (it gets too
# unwieldly here inside the makefile if esmf is prepended to everything.)
ESMF_FC        = $(FC)
ESMF_LINKER    = $(FLINKER)
ESMF_LINKOPTS  = $(LINKOPTS)
ESMF_LINKLIBS  = $(FLINKLIBS)
ESMF_CC        = $(CC)
ESMF_CXX       = $(CXX)
ESMF_CLINKER   = $(CLINKER)
ESMF_CLINKOPTS = $(LINKOPTS)
ESMF_CLINKLIBS = $(CLINKLIBS)

# these seem less useful, since if the user includes this file it already
# has a rule to make the .o files directly from the .F90 files.  but in case
# this file causes problems - e.g. conflict with other makefiles - then
# here are the flags - but it omits the Fixed/Free format, cpp vs not flags.
# those have to be added explicitly if not using our rules.

# collect all the compile flags together into single variable
ESMF_FLAGS = $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
              $(FCPPFLAGS) $(ESMF_INCLUDE)
ESMF_CFLAGS = $(COPTFLAGS) $(CFLAGS) $(CCPPFLAGS) $(ESMF_INCLUDE)

#-------------------------------------------------------------------------------

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
	if [ ! -f $(ESMFLIB) ]; then \
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
VPATH = $(ESMF_TOP_DIR)/$(LOCDIR):$(ESMF_TOP_DIR)/include

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
lib:  info info_h build_libs

build_libs: chkdir_lib include cppfiles
	cd $(ESMF_TOP_DIR) ;\
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
		$(RANLIB) $(LIBNAME); \
		$(RM) $(OBJS); \
	fi
	@if [ "$(QUICKSTART)" != "" ] ; then \
	   $(MAKE) -f $(MAKEFILE) tree_build_quick_start; fi


# copy private include files into src/include directory.
include: chkdir_include
	cd $(ESMF_TOP_DIR) ;\
	$(MAKE) ACTION=tree_include tree

# action for 'tree' target.
tree_include:
	@for hfile in ${STOREH} foo ; do \
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
CLEAN_DEFAULTS = *.o *.mod *.txt core ESM*.stdout ESM*.Log PET*.Log *ESMF_LogFile
CLEAN_TEXFILES = *.aux *.bbl *.blg *.log *.toc *.dvi *.ORIG

clean:
	$(MAKE) ACTION=tree_clean tree

# the GNU standard target is 'distclean' but we have had clobber in here
# for a long time, so for backward compatibility, leave them both.

distclean: clobber

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
# so check builds and runs the unit and system tests with EXHAUSTIVE pinned off.
# this does a cursory check, not a full, exhaustive check.
#
# 'gmake all_tests' makes and runs the full set of tests, respecting the user
# setting for EXHAUSTIVE.  it runs the unit tests, system tests, examples,
# and the demo.
#
# 'gmake validate' should probably do some numerical validation to make
# sure we have something like bit reproducibility, that we are not going to
# have wordsize problems, etc.   for now, we have no tests like that so it
# just runs the unit tests.
#

# quick sanity check, with an override to only do non-exhaustive tests
check:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) ESMF_EXHAUSTIVE=OFF info clean_check $(TEST_TARGETS_UNI) ;\
	else \
	  $(MAKE) ESMF_EXHAUSTIVE=OFF info clean_check $(TEST_TARGETS) ;\
        fi


build_check:
	$(MAKE) ESMF_EXHAUSTIVE=OFF build_unit_tests build_system_tests 


run_check:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) run_unit_tests_uni run_system_tests_uni ; \
	else \
	  $(MAKE) run_unit_tests run_system_tests ;\
        fi


clean_check:
	$(MAKE) clean_unit_tests clean_system_tests


# all tests, respecting user setting of EXHAUSTIVE
all_tests:
	@if [ $(ESMF_COMM) = "mpiuni" ] ; then \
	  $(MAKE) info $(ALLTEST_TARGETS_UNI) results_summary ;\
	else \
	  $(MAKE) info $(ALLTEST_TARGETS) results_summary ;\
        fi


build_all_tests:
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
	fi; \
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
	fi; \
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
        fi; \
	$(MAKE) ACTION=tree_build_system_tests tree ; \
	echo "ESMF system tests built successfully."

tree_build_system_tests:  $(SYSTEM_TESTS_BUILD) 


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
	$(FLINKER) $(LINKOPTS) -o $@ $(SYSTEM_TESTS_OBJ) $< $(FLINKLIBS)
	$(RM) -f *.o *.mod

#
# run_system_tests
#
run_system_tests:  reqdir_tests
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
	fi; \
	$(MAKE) ACTION=tree_run_system_tests tree ; \
	$(MAKE) check_system_tests

tree_run_system_tests: $(SYSTEM_TESTS_RUN) 

#
# run_system_tests_uni
#
run_system_tests_uni:  reqdir_tests
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
	$(MAKE) ACTION=tree_run_system_tests_uni tree ; \
	$(MAKE) check_system_tests

tree_run_system_tests_uni: $(SYSTEM_TESTS_RUN_UNI)

#
# this target deletes only the system test related files from the test subdir
#
clean_system_tests:
	$(RM) $(ESMF_TESTDIR)/*STest* 

#
# report statistics on system tests
#
check_system_tests: 
	@$(DO_ST_RESULTS)


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
build_unit_tests: reqfile_libesmf reqdir_lib chkdir_tests
	$(MAKE) config_unit_tests 
	$(MAKE) ACTION=tree_build_unit_tests tree
	@echo "ESMF unit tests built successfully."

tree_build_unit_tests: $(TESTS_BUILD)


$(ESMF_TESTDIR)/ESMF_%UTest : ESMF_%UTest.o $(ESMFLIB)
	$(FLINKER) $(LINKOPTS) -o $@  $(UTEST_$(*)_OBJS) $< $(FLINKLIBS)
	$(RM) -f *.o *.mod


$(ESMF_TESTDIR)/ESMC_%UTest : ESMC_%UTest.o $(ESMFLIB)
	$(CLINKER) $(LINKOPTS) -o $@  $(UTEST_$(*)_OBJS) $< $(CLINKLIBS)
	$(RM) -f *.o *.mod


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
	@if [ -f $(TESTS_CONFIG) ] ; then \
	   $(SED) -e 's/ [A-Za-z][A-Za-z]*processor/ Multiprocessor/' $(TESTS_CONFIG) > $(TESTS_CONFIG).temp; \
           $(MV) $(TESTS_CONFIG).temp $(TESTS_CONFIG); \
        fi
	-$(MAKE) ACTION=tree_run_unit_tests tree
	$(MAKE) check_unit_tests

tree_run_unit_tests: $(TESTS_RUN) 

#
# run_unit_tests_uni
#
run_unit_tests_uni:  reqdir_tests verify_exhaustive_flag
	@if [ -f $(TESTS_CONFIG) ] ; then \
	   $(SED) -e 's/ [A-Za-z][A-Za-z]*processor/ Uniprocessor/' $(TESTS_CONFIG) > $(TESTS_CONFIG).temp; \
           $(MV) $(TESTS_CONFIG).temp $(TESTS_CONFIG); \
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
	@echo "# This file used by test scripts, please do not delete." > $(TESTS_CONFIG)
ifeq ($(ESMF_EXHAUSTIVE),ON) 
ifeq ($(MULTI),) 
	@echo "Last built Exhaustive ;  Last run Noprocessor" >> $(TESTS_CONFIG)
else
	@echo "Last built Exhaustive ;  Last run" $(MULTI) >> $(TESTS_CONFIG)
endif
else
ifeq ($(MULTI),) 
	@echo "Last built Non-exhaustive ;  Last run Noprocessor" >> $(TESTS_CONFIG)
else
	@echo "Last built Non-exhaustive ;  Last run" $(MULTI) >> $(TESTS_CONFIG)
endif
endif

#
# verify that either there is no TESTS_CONFIG file, or if one exists that
# the string Exhaustive or Non-exhaustive matches the current setting of the
# ESMF_EXHAUSTIVE environment variable.  this is used when trying to run
# already-built unit tests, to be sure the user has not changed the setting
# of exhaustive and then assumed that it will take effect.  unfortunately at
# this time, the flag is compile-time and not run-time.   
#
verify_exhaustive_flag:
ifeq ($(ESMF_EXHAUSTIVE),ON) 
	@$(MAKE) UNIT_TEST_STRING="Exhaustive" exhaustive_flag_check
else
	@$(MAKE) UNIT_TEST_STRING="Non-exhaustive" exhaustive_flag_check
endif

exhaustive_flag_check:
	@if [ -s $(TESTS_CONFIG) -a \
	     `$(SED) -ne '/$(UNIT_TEST_STRING)/p' $(TESTS_CONFIG) | $(WC) -l` -ne 1 ] ; then \
	  echo "The ESMF_EXHAUSTIVE environment variable is a compile-time control for" ;\
          echo "whether a basic set or an exhaustive set of tests are built." ;\
	  echo "" ;\
	  echo "The current setting of ESMF_EXHAUSTIVE is \"$(ESMF_EXHAUSTIVE)\", which" ;\
	  echo "is not the same as when the unit tests were last built." ;\
	  echo "(This is based on the contents of the file:" ;\
          echo "$(TESTS_CONFIG) ";\
	  echo "which contains: `$(SED) -e '1d' $(TESTS_CONFIG)` )." ;\
	  echo "" ;\
	  echo "To rebuild and run the unit tests with the current ESMF_EXHAUSTIVE value, run:" ;\
	  echo "   $(MAKE) clean_unit_tests unit_tests"  ;\
	  echo "or change ESMF_EXHAUSTIVE to ON or OFF to match the build-time value." ;\
	  echo "" ;\
	  $(MAKE) err ;\
	fi

#
# this target deletes only the unit test related files from the test subdir
# so we can rebuild them with the proper flags if that is what is needed.
#
clean_unit_tests:
	$(RM) $(ESMF_TESTDIR)/*UTest* $(TESTS_CONFIG)


#
# report statistics on tests
#
check_unit_tests:
	@$(DO_UT_RESULTS)

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
	$(FLINKER) $(LINKOPTS) -o $@ $(EXAMPLE_$(*)_OBJS) $< $(FLINKLIBS)
	$(RM) -f *.o *.mod


$(ESMF_EXDIR)/ESMC_%Ex: ESMC_%Ex.o $(ESMFLIB)
	$(CLINKER) $(LINKOPTS) -o $@ $(EXAMPLE_$(*)_OBJS) $< $(CLINKLIBS)
	$(RM) $<

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
# this target deletes only the example related files from the example subdir
#
clean_examples:
	$(RM) $(ESMF_EXDIR)/*

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
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_demos tree

tree_demos: tree_build_demos tree_run_demos

demos_uni: build_libs chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_demos_uni tree

tree_demos_uni: tree_build_demos tree_run_demos_uni

#
# build_demos
#
build_demos: reqfile_libesmf reqdir_lib chkdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_build_demos tree
	@echo "ESMF demos built successfully."

tree_build_demos: $(DEMOS_BUILD) 

$(ESMF_TESTDIR)/%App : %Demo.o $(DEMOS_OBJ) $(ESMFLIB)
	$(FLINKER) $(LINKOPTS) -o $@ $(DEMOS_OBJ) $< $(FLINKLIBS)
	$(RM) -f *.o *.mod


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
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_run_demos tree

tree_run_demos: $(DEMOS_RUN) 

run_demos_uni:  reqdir_tests
	@if [ -d src/demo ] ; then cd src/demo; fi; \
	$(MAKE) ACTION=tree_run_demos_uni tree

tree_run_demos_uni: $(DEMOS_RUN_UNI) 

#
# this target deletes only the demos and output files created by the demos
#
clean_demos:
	$(RM) $(ESMF_TESTDIR)/*App 
	@if [ -d src/demo ] ; then cd src/demo; fi; \
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
	@echo "========================================="
	@echo "doc rule from common.mk"
	@echo "========================================="
	cd $(ESMF_TOP_DIR)/src/doc ;\
	$(MAKE) dvi html pdf
	@echo "Build doc completed."

alldoc: chkdir_doc include cppfiles tex
	@echo "========================================="
	@echo "Building All Documentation"
	@echo "========================================="
	@$(MAKE) dvi pdf html
	@echo "Build alldoc completed."

tex: chkdir_doc include cppfiles
	cd $(ESMF_TOP_DIR) ;\
	$(MAKE) ACTION=tree_tex tree

tree_tex: $(TEXFILES_TO_MAKE)

dvi: chkdir_doc include cppfiles tex
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


html: chkdir_doc include cppfiles tex
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
.SUFFIXES: .f .f90 .F .F90 $(SUFFIXES) .C .cc .cpp .r .rm .so

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
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
           $(F_FREECPP) $(FCPPFLAGS) $(ESMF_INCLUDE) $<

.F.o:
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
           $(F_FREENOCPP) $<

.f90.o:
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
           $(F_FIXCPP) $(FCPPFLAGS) $(ESMF_INCLUDE) $<

.f.o:
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) $(F_FIXNOCPP) $<

.c.o:
	$(CC) -c $(COPTFLAGS) $(CFLAGS) $(CCPPFLAGS) $(ESMF_INCLUDE) $<

.C.o:
	$(CXX) -c $(COPTFLAGS) $(CFLAGS) $(CCPPFLAGS) $(ESMF_INCLUDE) $<

.F90.a:
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
           $(F_FREECPP) $(FCPPFLAGS) $(ESMF_INCLUDE) $<
	$(AR) $(AR_FLAGS) $(LIBNAME) $*.o
	$(RM) $*.o

.F.a:
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
           $(F_FREENOCPP) $<
	$(AR) $(AR_FLAGS) $(LIBNAME) $*.o
	$(RM) $*.o

.f90.a:
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
           $(F_FIXCPP) $(FCPPFLAGS) $(ESMF_INCLUDE) $<
	$(AR) $(AR_FLAGS) $(LIBNAME) $*.o
	$(RM) $*.o

.f.a:
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) $(F_FIXNOCPP) $<
	$(AR) $(AR_FLAGS) $(LIBNAME) $*.o
	$(RM) $*.o

.c.a:
	$(CC) -c $(COPTFLAGS) $(CFLAGS) $(CCPPFLAGS) $(ESMF_INCLUDE) $<
	$(AR) $(AR_FLAGS) $(LIBNAME) $*.o
	$(RM) $*.o

.C.a:
	$(CXX) -c $(COPTFLAGS) $(CFLAGS) $(CCPPFLAGS) $(ESMF_INCLUDE) $<
	$(AR) $(AR_FLAGS) $(LIBNAME) $*.o
	$(RM) $*.o

# The rules below generate a valid Fortran file using gcc as a preprocessor.
# The -P option prevents putting #line directives in the output, and
# -E stops after preprocessing.  The 'tr' command substitutes one-for-one,
# translating @ into newline to separate lines in multiline macros (the output
# of the preprocessor is a single line which must be separated again), and
# also translates ^ into # so that other include files are ready to
# be processed by the second runthru of the preprocessor during the
# actual compile. (These lines are: ^include "fred.h" in the
# original source to shield them from the first preprocess pass.)
# the dir, notdir macros below are to be sure to create the .F90 file
# in the original source directory, since the makefile has already
# changed dirs into the mod dir to build.  The sed command removes
# any lines which start #pragma GCC .  these are generated by a couple
# versions of gcc and confuse the fortran compiler when trying to compile
# the newly generated file.

ifeq ($(origin CPPRULES),undefined)
.cpp.F90:
	$(CPP) -E -P -I$(ESMF_INCDIR) $< | tr "@^" "\n#" | \
              $(SED) -e '/^#pragma GCC/d' > $(dir $<)$(notdir $@)


.cpp.o:
	$(CPP) -E -P -I$(ESMF_INCDIR) $< | tr "@^" "\n#" | \
              $(SED) -e '/^#pragma GCC/d' > $(dir $<)$(basename $@).F90
	$(FC) -c $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
          $(F_FREECPP) $(FCPPFLAGS) $(ESMF_INCLUDE) $(dir $<)$(basename $@).F90
endif


#-------------------------------------------------------------------------------
#  Build shared library from regular lib (.so from .a)
#-------------------------------------------------------------------------------
shared:
	@if [ "$(SL_LIBS_TO_MAKE)" != "" ] ; then \
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
		    $(SL_LIBLINKER) $(SL_LIBOPTS) -o $(LDIR)/$$NEXTLIB.$(SL_SUFFIX) *.o $(SL_LIBLIBS) ;\
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
	@echo "========================================="
	@echo "_desdoc.dvi rule from common.mk"
	@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* des

%_refdoc.dvi : %_refdoc.ctex $(REFDOC_DEP_FILES)
	@echo "========================================="
	@echo "_refdoc.dvi rule from common.mk"
	@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* ref

%_reqdoc.dvi : %_reqdoc.ctex $(REQDOC_DEP_FILES)
	@echo "========================================="
	@echo "_reqdoc.dvi rule from common.mk"
	@echo "========================================="
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	$(DO_LATEX) $* req

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
$(ESMF_DOCDIR)/%_desdoc: %_desdoc.ctex $(DESDOC_DEP_FILES)
	@echo "========================================="
	@echo "_%desdoc from %.ctex rule from common.mk"
	@echo "========================================="
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	$(DO_L2H) $* des
	$(RM) .latex2html-init
	$(MV) -f $(@F) $(ESMF_DOCDIR)


$(ESMF_DOCDIR)/%_refdoc: %_refdoc.ctex $(REFDOC_DEP_FILES)
	@echo "========================================="
	@echo "_%refdoc from %.ctex rule from common.mk"
	@echo "========================================="
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	$(DO_L2H) $* ref
	$(RM) .latex2html-init
	$(MV) $(@F) $(ESMF_DOCDIR)

$(ESMF_DOCDIR)/%_reqdoc: %_reqdoc.ctex $(REQDOC_DEP_FILES)
	@echo "========================================="
	@echo "_%reqdoc from %.ctex rule from common.mk"
	@echo "========================================="
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	$(DO_L2H) $* req
	$(RM) .latex2html-init
	$(MV) $(@F) $(ESMF_DOCDIR)

#-------------------------------------------------------------------------------
#  These rules are for compiling the test examples.
#-------------------------------------------------------------------------------
.cpp.rm .cc.rm .C.rm .F.rm .f.rm .c.rm:
	-@$(RM) $* *.o $*.mon.* gmon.out mon.out


#-------------------------------------------------------------------------------
# Keep .o files
#-------------------------------------------------------------------------------
.PRECIOUS: %.o



