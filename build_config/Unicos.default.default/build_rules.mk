# $Id: build_rules.mk,v 1.8 2006/03/07 22:41:02 svasquez Exp $
# 
# Unicos.default.default
#

#
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
endif

ifeq ($(ESMF_COMM),mpi)
MPIRUN         = ${ESMF_TOP_DIR}/scripts/mpirun.unicos
endif

############################################################
#
# location of external libs.  if you want to use any of these,
# define ESMF_SITE to my_site so the build system can find it,
# copy this file into Linux.absoft.my_site, and uncomment the
# libs you want included.  remove the rest of this file since
# both this file and the site file will be included.

# LAPACK_INCLUDE   = 
# LAPACK_LIB       = -L/usr/local/lib -llapacko
# NETCDF_INCLUDE   = -I/opt/apptools/include
# NETCDF_LIB       = -L/opt/apptools/lib -lnetcdf
# HDF_INCLUDE      = -I /opt/apptools/include
# HDF_LIB          = -L /opt/apptools/lib -lmfhdf -ldf -ljpeg -lz
# BLAS_INCLUDE     = 
# BLAS_LIB         = -latlas

#
############################################################

#
# Location of MPI (Message Passing Interface) software  
#
ifeq ($(ESMF_COMM),mpi)
MPI_LIB        += -lmpi 
CFLAGS         += -DESMC_HAVE_INT_MPI_COMM
endif

############################################################

LD		   = ftn
RANLIB             = true

#
# C and Fortran compiler
#
C_CC		   = CC -h new_for_init -h ssp 
C_CXX		   = CC -h new_for_init -h ssp 
C_FC		   = ftn -h ssp -dy

C_CLINKER	   = CC -h ssp
C_FLINKER	   = CC -h ssp

# no extra libs needed to cross compile
C_CXXF90LIBS       =
C_F90CXXLIBS       =


#
# C, C++, and Fortran compiler 
#
C_FC_MOD           = -em -J
C_SLFLAG           =

C_CCV		   = cc -V
C_CXXV		   = CC -V
C_FCV              = ftn -V

#
F_FREECPP       = -f free -N 255 -F -M1549
F_FIXCPP        = -f fixed -N 132 -F -M1549
F_FREENOCPP     = -f free -N 255
F_FIXNOCPP      = -f fixed -N 132

###############################################################################

PARCH		   = Unicos

# no shared lib
SL_LIBS_TO_MAKE =
C_SL_LIBOPTS  =



###############################################################################
# common.mk overrides

# No gcc available, so use cpp.
CPP = cpp

# back in the common.mk file, make sure to not overwrite these rules
CPPRULES = defined

# change to not depend upon gcc -E -P behavior, remove -P and add filter 
# to delete #line
# amend bad cpp ".TRUE.", ". NOT." , "=>' output
# fix leading space that seems to show up on some lines
%.F90 : %.cpp
	${CPP} -E -I${ESMF_INCDIR} $< | tr "@^" "\n#" | sed -e 's/^ //' -e '/^#line/d' -e 's/ \. /./g' -e 's/\. not\./.not./g' -e 's/= >/=>/g' > $(dir$<)$(notdir $@)

%.o : %.cpp
	${CPP} -E -I${ESMF_INCDIR} $< | tr "@^" "\n#" | sed -e 's/^ //' -e '/^#line/d' -e 's/ \. /./g' -e 's/\. not\./.not./g' -e 's/= >/=>/g' > $(dir$<)$(notdir $@)
	${FC} -c ${C_FC_MOD}${ESMF_MODDIR} ${FOPTFLAGS} ${FFLAGS} ${F_FREECPP} ${FCPPFLAGS} ${ESMC_INCLUDE} $ (dir $<)$(basename $@) .F90

