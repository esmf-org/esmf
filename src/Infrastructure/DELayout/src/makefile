# $Id$

ALL: build_here 

# on the Cray X1 default template instantiation is a pain
ifeq ($(ESMF_OS),Unicos)
ifeq ($(ESMF_COMPILER),default)
ESMF_CXXCOMPILEOPTS += -h simple_templates
endif
endif

# XLC++ 8.0 on BGL systems has trouble with templates of recursive funcs
# in -O mode => use work-around in code.
ifeq ($(ESMF_OS),Linux)
ifeq ($(ESMF_COMPILER),xlf)
ESMF_CXXCOMPILEOPTS += -DBGLWORKAROUND
endif
endif

# If you have C files in this directory, list them all on the line below.
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

# on Cray XT/XE/XK with the Cray Compiler Suite (CCE) there is a
# problem compiling the local files with optimization ->
# solution: turn optimization off
ifeq ($(ESMF_OS),Unicos)
ifeq ($(ESMF_COMPILER),cce)
ESMF_OPTLEVEL = 0
endif
endif

# Leave the following line blank.
DIRS	  = 
