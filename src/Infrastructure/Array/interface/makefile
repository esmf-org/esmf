# $Id: makefile,v 1.10 2004/03/18 22:48:42 nscollins Exp $

# leave alone.
ALL: build_here 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_Array_F.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_Array.F90 ESMF_ArrayCreate.F90 \
             ESMF_ArrayGet.F90 ESMF_Alloc_C.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  =

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Arrayobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Array/interface


include $(ESMF_DIR)/makefile

# on the mac, you have to give this additional flag so the file uses > 16 bit
# offsets internally in the file.
ifeq ($(ESMF_COMPILER),absoft)
FFLAGS += -N11
endif

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = ESMF_ArrayCreate.F90 ESMF_ArrayGet.F90
CLOBBERDIRS =

ESMF_ArrayCreate.F90: ESMF_Array.F90

ESMF_ArrayGet.F90: ESMF_Array.F90

ESMF_ArrayCreate.F90: ESMF_ArrayCreate.cpp ESMF_ArrayCreateMacros.h

