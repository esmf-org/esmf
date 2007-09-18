# $Id: makefile,v 1.14 2007/09/18 22:21:14 cdeluca Exp $

# leave alone.
ALL: build_here 

AUTOGEN   = ESMF_LocalArrayCreate.F90 ESMF_LocalArrayGet.F90

SOURCEC	  = ESMC_LocalArray_F.C 
SOURCEF	  = $(AUTOGEN) ESMF_LocalArray.F90 ESMF_LocalAlloc_C.F90
SOURCEH	  = 
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
ESMF_F90COMPILEOPTS += -N11
endif

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

ESMF_LocalArrayCreate.F90: ESMF_LocalArrayCreate.cppF90 ESMF_LocalArrayMacros.h ESMF_LocalAllocMacros.h

ESMF_LocalArrayGet.F90: ESMF_LocalArrayGet.cppF90 ESMF_LocalArrayMacros.h ESMF_LocalAllocMacros.h

