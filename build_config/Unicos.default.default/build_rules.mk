# $Id: build_rules.mk,v 1.3 2004/10/28 22:21:22 nscollins Exp $
# 
# Unicos.default.default.mk
#

#
# Default MPI setting.
#
ifndef ESMF_COMM
export ESMF_COMM := mpi
endif
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
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
# BLAS usually comes with SGI. Do NOT use the parallel (library names with 
# mp in them) version of the SGI BLAS.
#
ifeq ($(ESMF_NO_IOCODE),true)
BLAS_LIB         =
LAPACK_LIB       =
NETCDF_LIB       = -lnetcdf_stubs
NETCDF_INCLUDE   = -I${ESMF_DIR}/src/Infrastructure/stubs/netcdf_stubs
HDF_LIB          =
HDF_INCLUDE      =
else
BLAS_LIB       = -latlas ${FC_LIB}
LAPACK_LIB     = -llapacko
NETCDF_LIB       = -L/opt/apptools/lib -lnetcdf
NETCDF_INCLUDE   = -I/opt/apptools/include
HDF_LIB          = -L /opt/apptools/lib -lmfhdf -ldf -ljpeg -lz
HDF_INCLUDE      = -I /opt/apptools/include
endif
# end of io bypass section

#
# Location of MPI (Message Passing Interface) software  
#
ifeq ($(ESMF_COMM),mpi)
ESMC_MPIRUN      = mpirun 
MPI_LIB        = -lmpi 
MPI_INCLUDE     = -DESMC_HAVE_INT_MPI_COMM
MPIRUN          = ${ESMC_MPIRUN}
endif

#
# The following is for mpiuni
#
ifeq ($(ESMF_COMM),mpiuni)
MPI_HOME        = ${ESMF_DIR}/src/Infrastructure/stubs/mpiuni
MPI_LIB         = -lmpiuni
MPI_INCLUDE     = -I${MPI_HOME}
MPIRUN          = ${MPI_HOME}/mpirun
endif

############################################################

LD		   = ftn
#
# C and Fortran compiler
#
C_CC		   = CC -h new_for_init -h ssp
C_FC		   = ftn -h ssp
C_CLINKER	   = cc -h ssp
C_FLINKER	   = ftn -h ssp
#
# C++ compiler
#
CXX_CC		   = CC -h new_for_init -h ssp
CXX_FC		   = ftn -h ssp
CXX_CLINKER	   = CC -h ssp
CXX_FLINKER	   = CC -h ssp
C_CXXF90LD         = CC -h ssp
C_F90CXXLD         = CC -h ssp
C_CXXF90LIBS       =
C_F90CXXLIBS       =
C_CXXSO		   = CC -h ssp


AR		   = ar
AR_FLAGS	   = cr
AR_EXTRACT         = -x
RM		   = rm -f
RANLIB		   = true
OMAKE		   = ${MAKE}
SHELL		   = /bin/sh
SED		   = /bin/sed
#
# C and Fortran compiler 
#
C_FC_MOD           = -em -J
C_CLINKER_SLFLAG   =
C_FLINKER_SLFLAG   =
C_CCV		   = cc -V -h ssp
C_FCV              = ftn -V -h ssp
C_SYS_LIB	   =
# ---------------------------- BOPT - g options ----------------------------
G_COPTFLAGS	   = -g 
G_FOPTFLAGS	   = -g 
# ----------------------------- BOPT - O options -----------------------------
O_COPTFLAGS	   =
O_FOPTFLAGS	   =
# ########################## Fortran compiler ##############################
#
F_FREECPP       = -f free -N 255 -F -M1549
F_FIXCPP        = -f fixed -N 132 -F -M1549
F_FREENOCPP     = -f free -N 255
F_FIXNOCPP      = -f fixed -N 132
# ########################## C++ compiler ##################################
#
CXX_CLINKER_SLFLAG =
CXX_FLINKER_SLFLAG =
CXX_CCV		   = CC -V
CXX_SYS_LIB	   =
# ------------------------- BOPT - g_c++ options ------------------------------
GCXX_COPTFLAGS	   = -g
GCXX_FOPTFLAGS	   = -g
# ------------------------- BOPT - O_c++ options ------------------------------
OCXX_COPTFLAGS	   =
OCXX_FOPTFLAGS	   =
# -------------------------- BOPT - g_complex options ------------------------
GCOMP_COPTFLAGS	   = -g 
GCOMP_FOPTFLAGS	   = -g
# --------------------------- BOPT - O_complex options -------------------------
OCOMP_COPTFLAGS	   =
OCOMP_FOPTFLAGS	   =
##################################################################################

PARCH		   = Unicos

SL_LIBS_TO_MAKE =

SL_SUFFIX   = so
SL_LIBOPTS  =
SL_LINKOPTS = 
SL_F_LINKER = $(F90CXXLD)
SL_C_LINKER = $(CXXF90LD)
SL_LIB_LINKER = $(CXXF90LD)

##### end common section


# common.mk overrides

# No gcc available, so use cpp.
CPP = cpp

# back in the common.mk file, make sure to not overwrite these rules
CPPRULES = defined

# change to not depend upon gcc -E -P behavior, remove -P and add filter to delete #line
# amend bad cpp ".TRUE.", ". NOT." , "=>' output
# fix leading space that seems to show up on some lines
%.F90 : %.cpp
	${CPP} -E -I${ESMF_INCDIR} $< | tr "@^" "\n#" | sed -e 's/^ //' -e '/^#line/d' -e 's/ \. /./g' -e 's/\. not\./.not./g' -e 's/= >/=>/g' > $(dir$<)$(notdir $@)

%.o : %.cpp
	${CPP} -E -I${ESMF_INCDIR} $< | tr "@^" "\n#" | sed -e 's/^ //' -e '/^#line/d' -e 's/ \. /./g' -e 's/\. not\./.not./g' -e 's/= >/=>/g' > $(dir$<)$(notdir $@)
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $ (dir $<)$(basename $@) .F90

