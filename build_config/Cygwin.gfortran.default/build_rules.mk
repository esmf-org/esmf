# $Id: build_rules.mk,v 1.10.2.1 2009/11/05 23:42:08 w6ws Exp $
#
# Cygwin.gfortran.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = gfortran-4
ESMF_CXXDEFAULT         = g++-4

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
ESMF_F90COMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpich)
# Mpich ----------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lpmpich -lmpich
ESMF_CXXDEFAULT         = mpiCC
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with gfortran) -----------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),msmpi)
# Microsofts version of MPICH2 on CCS 2003 is generally at:
# ESMF_MSMPIDIR        = /cygdrive/c/"Program Files"/"Microsoft Compute Cluster Pack"
# and on HPC 2008:
# ESMF_MSMPIDIR        = /cygdrive/c/"Program Files"/"Microsoft HPC Pack 2008 SDK"
ESMF_CXXCOMPILECPPFLAGS+= -D__int64="long long"
ESMF_F90COMPILEPATHS   += -I$(ESMF_MSMPIDIR)/Include
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_MSMPIDIR)/Include
ESMF_F90LINKLIBS       += $(ESMF_MSMPIDIR)/Lib/i386/msmpi.lib
ESMF_CXXLINKLIBS       += $(ESMF_MSMPIDIR)/Lib/i386/msmpi.lib
ESMF_MPIRUNDEFAULT      = mpiexec $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lmpi_cxx
ESMF_CXXDEFAULT         = mpicxx
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
ESMF_F90COMPILER_VERSION    = ${ESMF_F90COMPILER} -v --version
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -v --version

############################################################
# Cygwin 1.5.24 does not yet support POSIX IPC (memory mapped files)
#
ESMF_CXXCOMPILECPPFLAGS += -DESMF_NO_POSIXIPC
 
############################################################
# Fortran symbol convention
#
ifeq ($(ESMF_FORTRANSYMBOLS),default)
ESMF_F90COMPILEOPTS       += -fno-second-underscore
ESMF_F90LINKOPTS          += -fno-second-underscore
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_singleunderscore)
ESMF_F90COMPILEOPTS       += -fno-second-underscore
ESMF_F90LINKOPTS          += -fno-second-underscore
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_SINGLEUNDERSCORE
else
ifeq ($(ESMF_FORTRANSYMBOLS),lowercase_doubleunderscore)
ESMF_F90COMPILEOPTS       +=
ESMF_F90LINKOPTS          +=
ESMF_CXXCOMPILEOPTS       += -DESMF_LOWERCASE_DOUBLEUNDERSCORE
else
$(error "ESMF_FORTRANSYMBOLS = $(ESMF_FORTRANSYMBOLS)" not supported by ESMF and/or this platform)
endif
endif
endif

############################################################
# On IA64 set long and pointer types to 64-bit
#
ifeq ($(ESMF_ABI),64)
ESMF_CXXCOMPILEOPTS       += -march=k8 -m64 -mcmodel=medium
ESMF_CXXLINKOPTS          += -march=k8 -m64 -mcmodel=medium
ESMF_F90COMPILEOPTS       += -march=k8 -m64 -mcmodel=medium
ESMF_F90LINKOPTS          += -march=k8 -m64 -mcmodel=medium
endif

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -fopenmp
ESMF_OPENMP_CXXCOMPILEOPTS += -fopenmp
ESMF_OPENMP_F90LINKOPTS    += -fopenmp
ESMF_OPENMP_CXXLINKOPTS    += -fopenmp

############################################################
# Need this until the file convention is fixed (then remove these two lines)
#
ESMF_F90COMPILEFREENOCPP = -ffree-form
ESMF_F90COMPILEFIXCPP    = -cpp -ffixed-form

############################################################
# Determine where gcc's libraries are located
#
ESMF_F90LINKPATHS += \
  -L$(dir $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.a))
ESMF_F90LINKRPATHS += \
  -Wl,-rpath,$(dir $(shell $(ESMF_CXXCOMPILER) -print-file-name=libstdc++.a))

############################################################
# Determine where gfortran's libraries are located
#
ESMF_CXXLINKPATHS += \
  -L$(dir $(shell $(ESMF_F90COMPILER) -print-file-name=libgfortran.a))
ESMF_CXXLINKRPATHS += \
  -Wl,-rpath,$(dir $(shell $(ESMF_F90COMPILER) -print-file-name=libgfortran.a))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -lstdc++
ESMF_F90LINKOPTS += -Wl,--enable-auto-import

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lgfortran
ESMF_CXXLINKOPTS += -Wl,--enable-auto-import

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
