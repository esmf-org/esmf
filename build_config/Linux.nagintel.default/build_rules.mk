# $Id$
#
# Linux.nagintel.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = nagfor
ESMF_CXXDEFAULT         = icpc
ESMF_CDEFAULT           = icc

############################################################
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif

############################################################
# MPI dependent settings.
#
ifeq ($(ESMF_COMM),mpiuni)
# MPI stub library -----------------------------------------
ESMF_CPPFLAGS          += -DESMF_MPIUNI -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpich1)
# Mpich1 ---------------------------------------------------
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPICH1
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH1
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich)
# Mpich3 and up --------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with nag f95) ----------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ifeq ($(shell $(ESMF_DIR)/scripts/available mpifort),mpifort)
ESMF_F90DEFAULT         = mpifort
else
ESMF_F90DEFAULT         = mpif90
endif
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90LINKLIBS       += $(shell $(ESMF_DIR)/scripts/libs.openmpif90 $(ESMF_F90DEFAULT))
ESMF_CXXDEFAULT         = mpicxx
ESMF_CDEFAULT           = mpicc
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif
endif
endif
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v -V -dryrun
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -V -v -c
ESMF_CCOMPILER_VERSION      = ${ESMF_CCOMPILER} -V -v -c
ESMF_CXXMAJORVERSION        = $(shell $(ESMF_DIR)/scripts/version.intel 1 ${ESMF_CXXCOMPILER} -V)

############################################################
# Special debug flags
#
ESMF_CXXOPTFLAG_G       += -traceback

############################################################
# Set NAG unix modules when certain non-Standard system calls
# (e.g., ABORT) are made.
#
ESMF_F90COMPILEOPTS += -DESMF_NAG_UNIX_MODULE

############################################################
# Currently NAG does not support the Fortran2018 assumed type feature
#
ESMF_F90COMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_F2018ASSUMEDTYPE

############################################################
# Some ESMF tests fail for NAG with -O -> turn optimization off by default
#
ESMF_OPTLEVELDEFAULT  = 0

############################################################
# Set kind numbering system to "byte"
#
# ESMF_F90COMPILEOPTS += -kind=byte

############################################################
# Set f95 to be more premissive and issue warning before error
#
ESMF_F90COMPILEOPTS += -dusty

############################################################
# Conditionally add pthread compiler and linker flags
#
ifeq ($(ESMF_PTHREADS),ON)
ESMF_F90COMPILEOPTS += -thread_safe
ESMF_F90LINKOPTS    += -thread_safe
ESMF_CXXCOMPILEOPTS += -pthread
ESMF_CXXLINKOPTS    += -pthread
endif

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -openmp
ESMF_OPENMP_F90LINKOPTS    += -openmp
ifeq ($(shell [ $(ESMF_CXXMAJORVERSION) -ge 16 ] && echo true), true)
ESMF_OPENMP_CXXCOMPILEOPTS += -qopenmp
ESMF_OPENMP_CXXLINKOPTS    += -qopenmp
else
ESMF_OPENMP_CXXCOMPILEOPTS += -openmp
ESMF_OPENMP_CXXLINKOPTS    += -openmp
endif

############################################################
# Explicit flags for handling specific format and cpp combos
#
ESMF_F90COMPILEFREENOCPP = -free
ESMF_F90COMPILEFIXCPP    = -fixed -fpp

############################################################
# Set rpath syntax
#
ESMF_F90RPATHPREFIX         = -Wl,-Wl,,-rpath,,
ESMF_CXXRPATHPREFIX         = -Wl,-rpath,
ESMF_CRPATHPREFIX           = -Wl,-rpath,

############################################################
# Determine where icpc's libraries are located
#
ESMF_F90LINKPATHS += $(addprefix -L,$(shell $(ESMF_DIR)/scripts/libpath.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)"))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += $(shell $(ESMF_DIR)/scripts/libs.icpc "$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEOPTS)") -lrt -ldl

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lrt -ldl $(shell $(ESMF_DIR)/scripts/libs.nag $(ESMF_F90COMPILER))

############################################################
# Linker option that ensures that the specified libraries are 
# used to also resolve symbols needed by other libraries.
#
ESMF_F90LINKOPTS          += -Wl,-Wl,,--no-as-needed
ESMF_CXXLINKOPTS          += -Wl,--no-as-needed

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
