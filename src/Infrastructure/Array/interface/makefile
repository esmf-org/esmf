# $Id: makefile,v 1.13 2004/08/03 19:33:41 nscollins Exp $

# leave alone.
ALL: build_here 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# files which are generated from .cpp files
AUTOGEN   = ESMF_ArrayCreate.F90 ESMF_ArrayGet.F90

SOURCEC	  = ESMC_Array_F.C
SOURCEF	  =  ESMF_Array.F90 $(AUTOGEN) ESMF_Alloc_C.F90
SOURCEH	  =
CPPFILES  = $(AUTOGEN)

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

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
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

ESMF_ArrayCreate.o: ESMF_ArrayCreate.cpp ESMF_ArrayCreateMacros.h ESMF_Array.o

ESMF_ArrayGet.o: ESMF_ArrayGet.cpp ESMF_ArrayGetMacros.h ESMF_ArrayCreate.o

