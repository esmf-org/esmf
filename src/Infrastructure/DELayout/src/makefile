# $Id: makefile,v 1.11 2008/09/26 04:16:47 cdeluca Exp $

# leave alone.
ALL: build_here 

# on the Cray X1 default template instantiation is a pain
ifeq ($(ESMF_OS),Unicos)
ifeq ($(ESMF_COMPILER),default)
ESMF_CXXCOMPILEOPTS += -h simple_templates
endif
endif

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMCI_DELayout.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_DELayout.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/DELayout/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 

# XLC++ on BGL systems has trouble with templates of recursive funcs in -O mode
# -> this seems to be a compiler bug and we may be able to take this out once
# IBM has it fixed.
ifeq ($(ESMF_OS),Linux)
ifeq ($(ESMF_COMPILER),xlf)
ESMF_OPTLEVEL = 0
endif
endif
