# $Id: makefile,v 1.8 2004/03/23 18:17:23 nscollins Exp $

# leave alone.
ALL: build_here 

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_LocalArray_F.C 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_LocalArray.F90 ESMF_LocalAlloc_C.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# include files which go into the public src dir
STOREH	  = 

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
CLEANFILES  = ESMF_LocalArray.F90
CLOBBERDIRS =

ESMF_LocalAlloc_C.o:  ESMF_LocalArray.o

ESMF_LocalArray.F90: ESMF_LocalArray.cpp ESMF_LocalArrayMacros.h

