# $Id$ 

ALL: tracelib_preload tracelib_static

SOURCEC	  = preload.c wrappers_io.c
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


tracelib_preload: preload.o wrappers_io.o
	$(ESMF_SL_LIBLINKER) $(ESMF_SL_LIBOPTS) -o $(ESMF_LDIR)/libesmftrace_preload.$(ESMF_SL_SUFFIX) *.o $(ESMF_SL_LIBLIBS)

tracelib_static: wrappers_io.o
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(ESMF_LDIR)/libesmftrace_static.a wrappers_io.o
