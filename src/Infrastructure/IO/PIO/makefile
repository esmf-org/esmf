# $Id$

# locally disable parallel build:
.NOTPARALLEL:

ALL: build_here

ifdef ESMF_NETCDF
ESMF_CPPFLAGS += -D_NETCDF
endif

ifdef ESMF_PNETCDF
ESMF_CPPFLAGS += -D_PNETCDF
endif

ESMF_CPPFLAGS += -DNO_MPIMOD

SOURCEC	  = topology.c \
	piocpp.C
SOURCEF	  = \
	pio_kinds.F90		pio_types.F90		pio_support.F90 \
	pio_utils.F90		pio_spmd_utils.F90	\
	alloc_mod.F90		pionfread_mod.F90	\
	ionf_mod.F90		pio_msg_mod.F90		\
	nf_mod.F90		pionfwrite_mod.F90	\
	pio_mpi_utils.F90	iompi_mod.F90		pionfget_mod.F90 \
	pionfput_mod.F90	pio_nf_utils.F90	\
	calcdecomp.F90		calcdisplace_mod.F90	\
	box_rearrange.F90	\
	pionfatt_mod.F90	\
	rearrange.F90		piodarray.F90	\
	piolib_mod.F90		\
	pio.F90 		\
	pio_msg_callbacks.F90	pio_msg_getput_callbacks.F90 \
	pio_cpp_utils.F90	pio_cpp_binding.F90	darray_cpp_binding.F90 \
	nf_cpp_binding.F90

SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IO/PIO

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
