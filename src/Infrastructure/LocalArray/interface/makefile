# $Id: makefile,v 1.10 2004/08/03 19:33:49 nscollins Exp $

# leave alone.
ALL: build_here 

AUTOGEN   = ESMF_LocalArray.F90

SOURCEC	  = ESMC_LocalArray_F.C 
SOURCEF	  = $(AUTOGEN) ESMF_LocalAlloc_C.F90
SOURCEH	  = 
STOREH	  = 
CPPFILES  = $(AUTOGEN)

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Dataobj
LIBBASE	  = libesmf


# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/LocalArray/interface


include $(ESMF_DIR)/makefile

# on the absoft compiler on the mac, files in this dir have to have
# > 16 bit internal jump tables.  see the absoft faq's for more info.
ifeq ($(ESMF_COMPILER),absoft)
FFLAGS += -N11
endif

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

ESMF_LocalAlloc_C.o:  ESMF_LocalArray.o

ESMF_LocalArray.o: ESMF_LocalArray.cpp ESMF_LocalArrayMacros.h

