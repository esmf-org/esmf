# $Id: makefile,v 1.12 2008/09/26 05:53:53 cdeluca Exp $

ALL: build_here 

# on the Cray X1 default template instantiation is a pain
ifeq ($(ESMF_OS),Unicos)
ifeq ($(ESMF_COMPILER),default)
ESMF_CXXCOMPILEOPTS += -h simple_templates
endif
endif

SOURCEC	  = ESMCI_DELayout.C
SOURCEF	  = 
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_DELayout.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
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
