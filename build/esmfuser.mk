#  $Id: esmfuser.mk,v 1.1 2005/12/16 20:48:40 nscollins Exp $
#===============================================================================
#
#  GNUmake makefile fragment - intended to be included in an ESMF user's
#                              makefile.
#
#===============================================================================

#-------------------------------------------------------------------------------
#  If environment variables are not set give them default values.
#  For some variables having the literal string 'default' is ok; 
#  for others, look for this string and override it the same as 
#  if it was unset originally.
#-------------------------------------------------------------------------------

ESMF_TOP_DIR = $(ESMF_DIR)

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

# netcdf calls are always referenced from the esmf library, so if the user
# does not have them, we supply a stub library which simply prints an error
# if they are called.  plus, we need to compile out some of the calls to
# the I/O code - so define this badly named symbol.  The FPP line is
# commented out because i am experimenting with passing all CPP symbols
# to FPP so we do not have to set them both places.
ifndef NETCDF_LIB
NETCDF_INCLUDE   = -I$(ESMF_DIR)/src/Infrastructure/stubs/netcdf_stubs
NETCDF_LIB       = -lnetcdf_stubs
##FPPFLAGS       += $(FPP_PREFIX)-DESMF_NO_IOCODE
CPPFLAGS       += -DESMF_NO_IOCODE
export ESMF_NO_IOCODE = true
endif

# at this point netcdf always has a value - either from the user pointing
# to the real lib location, or our stub lib.
EXTRA_INCLUDES += $(NETCDF_INCLUDE)
EXTRA_LIBS += $(NETCDF_LIB)


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
#LD_PATHS  = $(SLFLAG)$(LDIR) $(C_LD_PATHS)
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
# common.mk file, then they can use these to link with.
ESMF_FC           = $(FC)
ESMF_COMPILEFLAGS = $(FC_MOD)$(ESMF_MODDIR) $(FOPTFLAGS) $(FFLAGS) \
                    $(FCPPFLAGS) $(ESMF_INCLUDE)
ESMF_LINKER       = $(FLINKER)
ESMF_LINKFLAGS    = $(LINKOPTS)
ESMF_LINKLIBS     = $(FLINKLIBS)

ESMF_CC            = $(CC)
ESMF_CXX           = $(CXX)
ESMF_CCOMPILEFLAGS = $(COPTFLAGS) $(CFLAGS) $(CCPPFLAGS) $(ESMF_INCLUDE)
ESMF_CLINKER       = $(CLINKER)
ESMF_CLINKFLAGS    = $(LINKOPTS)
ESMF_CLINKLIBS     = $(CLINKLIBS)



