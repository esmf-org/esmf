# $Id$ 

ALL: build_here 

SOURCEC	  = esmftrc.c ESMCI_Trace.C ESMCI_TraceWrap.C ESMCI_TraceMetadata.C
SOURCEF	  = 
SOURCEH	  = esmftrc.h ESMCI_Trace.h ESMCI_HashMap.h ESMCI_HashNode.h ESMCI_KeyHash.h
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/Trace/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 


