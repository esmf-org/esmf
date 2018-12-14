# $Id$ 

ALL: build_here 

SOURCEC	  = ESMCI_RunSeqDGraph.C ESMCI_GraphDotUtils.C ESMCI_RunSeqUtils.C ESMCI_Mapper.C
SOURCEF	  = 
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/Mapper/src

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

ESMF_CXXCOMPILEOPTS += -std=c++11
