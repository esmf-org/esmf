# $Id$ 

ALL: build_here 

SOURCEC	  = esmftrc.c ESMCI_Trace.C ESMCI_TraceWrap.C ESMCI_TraceMetadata.C ESMCI_TraceClock.C
SOURCEF	  = 
SOURCEH	  = esmftrc.h ESMCI_Trace.h ESMCI_TraceUtil.h ESMCI_HashMap.h ESMCI_HashNode.h 
SOURCEH  += ESMCI_KeyHash.h ESMCI_RegionNode.h ESMCI_ComponentInfo.h ESMCI_TraceRegion.h ESMCI_RegionSummary.h
STOREH    = ESMCI_TraceRegion.h ESMF_TraceRegion.inc ESMCI_TraceMacros.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Trace/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 


