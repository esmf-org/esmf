# $Id$ 

ALL: build_here 

AUTOGEN   = 
SOURCEC	  = 
SOURCEF	  = ESMF_XGrid.F90 ESMF_XGridGet.F90 ESMF_XGridCreate.F90
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/XGrid/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

# on AIX with the new XLF compiler (version 10.1) there is a
# problem compiling the local files with optimization ->
# solution: turn optimization off
ifeq ($(ESMF_OS),AIX)
ESMF_OPTLEVEL = 0
endif

# on IBM BGL with the XLF compiler there is a
# problem compiling the local files with optimization ->
# solution: turn optimization off
ifeq ($(ESMF_OS),Linux)
ifeq ($(ESMF_COMPILER),xlf)
ESMF_OPTLEVEL = 0
endif
endif

DIRS	  = 

