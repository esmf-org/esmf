#  $Id: build_rules.mk,v 1.10 2004/12/17 21:01:39 nscollins Exp $
#
#  Darwin.xlf.default.mk
#
# if you have xlf and gcc/g++, this should work.
# if you also have xlc/xlC and want to use them, then:
#   setenv ESMF_C_COMPILER xlc
# before building.
#

#
#  Make sure that ESMF_PREC is set to 32
#
ESMF_PREC = 32

#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpiuni
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpiuni
endif

#
# default C compiler setting.
#
ifndef ESMF_C_COMPILER
export ESMF_C_COMPILER := gcc
endif

############################################################
#
#  The following naming convention is used:
#     XXX_LIB - location of library XXX
#     XXX_INCLUDE - directory for include files needed for library XXX
#
# Location of BLAS and LAPACK.  See ${ESMF_DIR}/docs/instllation.html
# for information on retrieving them.
#
#
ifeq ($(ESMF_NO_IOCODE),true)
BLAS_LIB         =
LAPACK_LIB       =
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB         = -L/sw/lib -latlas
LAPACK_LIB       = -L/sw/lib -llapack
NETCDF_LIB       = -L/sw/lib -lnetcdf
NETCDF_INCLUDE   = -I/sw/include
HDF_LIB          = -L/sw/lib/ -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I/sw/include
endif

# Location of MPI (Message Passing Interface) software

# conditional sections, depending on whether you have lam, mpich,
# or neither and want to use the bypass uniprocessor mpi stub library.
# if these locations don't match your installation location, edit them
# to correspond to where the files are located.
# setenv ESMF_COMM to select the proper section before building.

ifeq ($(ESMF_COMM),lam)
# with lam-mpi installed in /usr/local:
MPI_HOME       =
MPI_LIB        = -lmpi -llam
MPI_INCLUDE    =
MPIRUN         =  mpirun
endif

ifeq ($(ESMF_COMM),mpich)
# with mpich installed in /usr/local:
ESMC_MPIRUN      = mpirun
MPI_HOME       =  /usr/local
MPI_LIB        = -lmpich -lpmpich
MPI_INCLUDE    = -I${MPI_HOME}/include -DESMF_MPICH=1
MPIRUN         =  ${MPI_HOME}/bin/mpirun
endif

ifeq ($(ESMF_COMM),mpiuni)
# without mpich installed:
MPI_HOME       = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
MPI_LIB        = -lmpiuni
MPI_INCLUDE    = -I${MPI_HOME}
MPIRUN         =  ${MPI_HOME}/mpirun
endif

#
# standard commands
#
AR			= ar
AR_FLAGS		= cr
AR_EXTRACT              = -x
RM			= rm -f
OMAKE			= ${MAKE}
RANLIB			= ranlib
SHELL			= /bin/sh
SED			= /usr/bin/sed


#
# Common compiler flags
#
COM_MEMCHECK_FLAG      = 
COM_FULLPATH_FLAG      =
COM_DEBUG_FLAG         = -g
COM_ALL_DEBUG_FLAGS    = -g $(COM_MEMCHECK_FLAG) $(COM_FULLPATH_FLAG)
COM_MAXMEM_FLAG        = -qmaxmem=4000
COM_NOWARN_FLAG        = -w
COM_SPILL_FLAG         = -qspill=3000
COM_OPT_FLAG           = -O2
COM_ALL_OPT_FLAGS      = -O2 $(COM_MAXMEM_FLAG) $(COM_NOWARN_FLAG) $(COM_SPILL_FLAG)
COM_PLAIN_FLAG         =

#
# C compiler flags
#
RESTRICTED_POINTERS	= -qkeyword=restrict
NO_AUTO_PARALLEL	= -qsmp=noauto
NO_INLINING		= -Q
NO_LINE_DIRECTIVES	= -P

#
# Fortran compiler flags
#
REAL8			= -qrealsize=8
STRICT			= -qstrict
#FPPOPTS                += -qcclines -qinit=f90ptr
FPP_PREFIX		= -WF,
FPPFLAGS		= $(addprefix $(FPP_PREFIX), $(FPPOPTS))
FPPFLAGS		+= $(addprefix $(FPP_PREFIX), $(CPPFLAGS))
F_FREECPP               = -qfree=f90 -qsuffix=cpp=F90
F_FIXCPP                = -qfixed=132 -qsuffix=cpp=f90
F_FREENOCPP             = -qfree=f90 -qsuffix=f=F
F_FIXNOCPP              = -qfixed=132 -qsuffix=f=f        

#
# compiler section
#
ifeq ($(ESMF_COMM),mpich)
# with mpich, have to call wrappers instead of compilers directly
# for now, assume they are built with gcc settings and not around xlc
C_CC			= mpicc
C_CCV			= $(C_CC) --version
CXX_CC			= mpic++
CXX_CCV			= $(CXX_CC) --version
C_FC			= mpif90 
C_FCV			= $(C_FC) --version

CXX_LIB_PATH            =
CXX_SYS_LIB		= $(CXX_LIB_PATH) -lxlf_r -lxlf90_r  \
                           -lcomplex -lisode -lstdc++
C_CXXF90LIBS		= -L. $(CXX_LIB_PATH) -lm -lxlf90_r -lstdc++
C_F90CXXLIBS		= -L. $(CXX_LIB_PATH) -lxlf90_r -lstdc++


else
# non-mpich section: lam or mpiuni

ifeq ($(ESMF_C_COMPILER),gcc)
# default is to use gcc/g++ for the compiling C files
C_CC			= gcc
C_CCV			= $(C_CC) -v
CXX_CC			= g++
CXX_CCV			= $(CXX_CC) -v
CXX_LIB_PATH            =
CXX_SYS_LIB		= $(CXX_LIB_PATH) -lxlf_r -lxlf90_r  \
                           -lcomplex -lisode -lstdc++
C_CXXF90LIBS		= -L. $(CXX_LIB_PATH) -lm -lxlf90_r -lstdc++
C_F90CXXLIBS		= -L. $(CXX_LIB_PATH) -lxlf90_r -lstdc++

else
# if you have the ibm xlc/xlC product, setenv ESMF_C_COMPILER xlc first
C_CC			= xlc_r 
C_CCV			= which $(C_CC)
CXX_CC			= xlC_r 
CXX_CCV			= which $(CXX_CC)
CXX_LIB_PATH            = -L/opt/ibmcmp/vacpp/6.0/lib/
CXX_SYS_LIB		= $(CXX_LIB_PATH) -lxlf_r -lxlf90_r  \
                           -lcomplex -lisode -libmc++ -lstdc++
C_CXXF90LIBS		= -L. $(CXX_LIB_PATH) -lm -lxlf90_r -libmc++ -lstdc++
C_F90CXXLIBS		= -L. $(CXX_LIB_PATH) -lxlf90_r -libmc++ -lstdc++

# end of xlc section
endif


#
# xlf Fortran compiler from ibm for mac os x
#
C_FC			= xlf95_r 
C_FCV			= which $(C_FC)

# end of non-mpich section
endif

# settings independent of which compiler is selected

CXX_FC			= $(C_FC)

C_FC_MOD		= -I
C_CLINKER		= $(C_CC)
C_FLINKER		= $(C_FC)
CXX_CLINKER		= $(CXX_CC)
CXX_FLINKER		= $(CXX_FC) 

C_SYS_LIB		= -lxlf_r -llf90_r  -lisode

SLFLAG                  = -L
C_CLINKER_SLFLAG	= $(SLFLAG)
C_FLINKER_SLFLAG	= $(SLFLAG)
CXX_CLINKER_SLFLAG	= $(SLFLAG)
CXX_FLINKER_SLFLAG	= $(SLFLAG)

FCPPFLAGS		= ${ESMC_INCLUDE} ${PCONF} ${ESMC_PARCH} ${FPPFLAGS} $(FCPP_EXHAUSTIVE)


# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
G_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
GCXX_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
GCXX_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
GCOMP_COPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)
GCOMP_FOPTFLAGS		= $(COM_ALL_DEBUG_FLAGS)

# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
O_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
OCXX_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
OCXX_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)
OCOMP_COPTFLAGS		= $(COM_ALL_OPT_FLAGS)
OCOMP_FOPTFLAGS		= $(COM_OPT_FLAG) $(COM_WARN_FLAG)

C_CXXF90LD		= ${CXX_CC}
C_F90CXXLD		= ${CXX_FC}


C_CXXSO			= $(CXX_CC) -G
C_CXXSOLIBS		= -L. $(CXX_LIB_PATH) -lm -lxlf90_r -libmc++ -lstdc++

PARCH			= mac_osx

SL_LIBS_TO_MAKE =

SL_SUFFIX   = so
SL_LIBOPTS  = -G -qmkshrobj $(C_F90CXXLIBS) $(MPI_LIB)
SL_LINKOPTS = 
SL_LIB_LINKER = $(CXX_CC) -L$(ESMF_LIBDIR)
SL_F_LINKER = $(F90CXXLD) 
SL_C_LINKER = $(CXXF90LD)

# SL_SUFFIX   =
# SL_LIBOPTS  =
# SL_LINKOPTS =
# SL_F_LINKER = $(F90CXXLD) 
# SL_C_LINKER = $(CXXF90LD) 


