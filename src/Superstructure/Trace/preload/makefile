# $Id$ 

ALL: shared_preload

SOURCEC	  = preload.c
SOURCEF	  = 
SOURCEH	  = 
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/Trace/preload

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 


shared_preload: preload.o
	$(ESMF_SL_LIBLINKER) $(ESMF_SL_LIBOPTS) -o $(ESMF_LDIR)/libesmfpreload.$(ESMF_SL_SUFFIX) *.o $(ESMF_SL_LIBLIBS)
