# $Id: build_rules.mk,v 1.13.2.1 2008/07/16 00:19:13 theurich Exp $
#
# Linux.sxcross.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = sxmpif90
ESMF_CXXDEFAULT         = sxmpic++
ESMF_ARDEFAULT          = sxar

############################################################
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
endif

############################################################
# MPI dependent settings.
#
ifeq ($(ESMF_COMM),mpiuni)
# MPI stub library -----------------------------------------
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpi)
# Vendor MPI -----------------------------------------------
ESMF_MPIRUNDEFAULT      = mpirun.sx
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -EP -dW -V
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V

############################################################
# NEC SX is 64-bit, need to set it here because cross compiling in Linux
#
export ESMF_ABI := 64

############################################################
# Set NEC SX specific compiler and linker options
#
ESMF_CXXCOMPILEOPTS       += -K exceptions -D_REENTRANT
ESMF_CXXLINKOPTS          += -K exceptions -D_REENTRANT
ESMF_F90COMPILEOPTS       += -EP -dW -Pmulti -Wf,-pvctl vwork=stack
ESMF_F90COMPILEOPTS       += -Wf,"-L fmtlist,map,objlist,summary,transform"
ESMF_F90LINKOPTS          += -EP -dW -Pmulti -Wf,-pvctl vwork=stack
ESMF_F90LINKOPTS          += -Wf,"-L fmtlist,map,objlist,summary,transform"

############################################################
# NEC SX compute nodes do not have support for POSIX IPC (memory mapped files)
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC

############################################################
# NEC SX compute nodes do not have support for "gethostid()"
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_GETHOSTID

############################################################
# NEC SX compute nodes do not have support for "nanosleep()"
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_NANOSLEEP

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90LINKLIBS    += -lpthread
ESMF_CXXLINKLIBS    += -lpthread
endif

############################################################
# NEC SX hardcoded paths to system libs
#
ESMF_CXXLINKPATHS   += -L/SX/usr/lib
ESMF_CXXLINKRPATHS  =
ESMF_F90LINKPATHS   += -L/SX/usr/lib
ESMF_F90LINKRPATHS  =

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lC++_eh -lcpp

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lf90sxe -lm90sxe -li90sx -lC++_eh -lm

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =

############################################################
# Exclude I1 and I2 data kinds
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_INTEGER_1_BYTE
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_INTEGER_2_BYTE

############################################################
# Require special treatment of I8 on NEC SX
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NEC_KIND_I8
