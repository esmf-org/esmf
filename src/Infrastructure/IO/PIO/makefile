# $Id: makefile,v 1.4 2010/06/09 21:41:39 theurich Exp $

ALL: build_here 

CPPFLAGS += -D_USEBOX

#TODO: set following flag conditionally once ESMF build configs handle MPI2
CPPFLAGS += -DNO_MPI2

ifneq ($(ESMF_COMM),mpiuni)
CPPFLAGS += -DUSEMPIIO
endif

ifdef ESMF_NETCDF
CPPFLAGS += -D_NETCDF
endif

ifdef ESMF_PNETCDF
CPPFLAGS += -D_PNETCDF
endif

#TODO: Remove the following conditional that turns PIO compilation off for 
#TODO: mpiuni, once we have a solution in place that allows us to compile the
#TODO: PIO sources under ESMF with mpiuni.
ifneq ($(ESMF_COMM),mpiuni)
SOURCEC	  = pnetcdfversion.c topology.c
SOURCEF	  = pio_kinds.F90 pio_types.F90 pio_support.F90 alloc_mod.F90 \
            pio_utils.F90 ionf_mod.F90 pionfread_mod.F90 nf_mod.F90 \
            pionfwrite_mod.F90 pio_mpi_utils.F90 iompi_mod.F90 \
            pio_quicksort.F90 mct_rearrange.F90 pio_spmd_utils.F90 \
            box_rearrange.F90 rearrange.F90 piolib_mod.F90 piodarray.F90 \
            pionfatt_mod.F90 pionfput_mod.F90 pionfget_mod.F90 pio.F90 \
            pio_nf_utils.F90
else
SOURCEC	  = 
SOURCEF	  = 
endif
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IO/PIO

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 

