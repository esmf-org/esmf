# $Id: makefile,v 1.13 2012/07/23 19:28:31 gold2718 Exp $

# locally disable parallel build:
.NOTPARALLEL:

ALL: build_here

CPPFLAGS += -D_USEBOX

#TODO: Use correct test variable once ESMF build configs handle MPI2
ifdef NO_MPI2
  CPPFLAGS += -DNO_MPI2
  @echo -e "disabling MPI-2 calls"
endif

ifdef ESMF_MPIIO
CPPFLAGS += -DUSEMPIIO -DNO_SIZEOF
endif

ifdef ESMF_NETCDF
CPPFLAGS += -D_NETCDF
endif

ifdef ESMF_PNETCDF
CPPFLAGS += -D_PNETCDF
endif

CPPFLAGS += -DNO_MPIMOD

# - only turn internal PIO compilation on if that is requested
SOURCEC   = topology.c          \
           piocpp.C
SOURCEF   = pio_kinds.F90        pio_types.F90        pio_support.F90         \
            alloc_mod.F90        pio_utils.F90        ionf_mod.F90            \
            pionfread_mod.F90    pio_msg_mod.F90      nf_mod.F90              \
            pionfwrite_mod.F90   pio_mpi_utils.F90    iompi_mod.F90           \
            pio_quicksort.F90    pio_spmd_utils.F90   calcdisplace_mod.F90    \
            box_rearrange.F90    rearrange.F90        calcdecomp.F90          \
            piodarray.F90        piolib_mod.F90       pionfatt_mod.F90        \
            pionfput_mod.F90     pionfget_mod.F90     pio_nf_utils.F90        \
            pio.F90              pio_msg_callbacks.F90                        \
            pio_msg_getput_callbacks.F90                                      \
            pio_cpp_utils.F90    pio_cpp_binding.F90  nf_cpp_binding.F90      \
            darray_cpp_binding.F90

SOURCEH     = 
# List all .h files which should be copied to common include dir
STOREH      = 

OBJSC       = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF       = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE     = libesmf
LOCDIR      = src/Infrastructure/IO/PIO

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

# Dependencies
piocpp.o: piocpp.C pio.h pio_kinds.h

include ${ESMF_DIR}/makefile

DIRS        = 
