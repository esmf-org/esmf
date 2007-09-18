# $Id: makefile,v 1.4 2007/09/18 22:21:10 cdeluca Exp $

# leave alone.
ALL: build_here 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# files which are generated from .cpp files
AUTOGEN   = ESMF_InternArrayCreate.F90 ESMF_InternArrayGet.F90

SOURCEC	  = ESMC_InternArrayDataMap.C ESMC_InternArray_F.C
SOURCEF	  = ESMF_InternArrayDataMap_C.F90 ESMF_InternArray.F90 $(AUTOGEN) ESMF_Alloc_C.F90
SOURCEH	  =
STOREH    = ESMC_InternArrayDataMap.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

# is MANSEC something about documentation?
MANSEC	  = InternArrayobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/InternArray/interface


include $(ESMF_DIR)/makefile

# on the mac, you have to give this additional flag so the file uses > 16 bit
# offsets internally in the file.
ifeq ($(ESMF_COMPILER),absoft)
ESMF_F90COMPILEOPTS += -N11
endif

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

ESMF_InternArrayCreate.o: ESMF_InternArrayCreate.cppF90 ESMF_InternArrayCreateMacros.h ESMF_InternArray.o

ESMF_InternArrayGet.o: ESMF_InternArrayGet.cppF90 ESMF_InternArrayGetMacros.h ESMF_InternArrayCreate.o

