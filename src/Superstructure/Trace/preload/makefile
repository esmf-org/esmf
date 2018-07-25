# $Id$ 

ALL: tracelib_preload tracelib_static

SOURCEC	  = preload.C preload_io.C preload_mpi.C wrappers.C wrappers_io.C wrappers_mpi.C
SOURCEF	  = 
SOURCEH	  = 
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/Trace/preload

CLEANDIRS   =
CLEANFILES  = $(ESMF_LDIR)/libesmftrace_* 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

#ESMF_CXXCOMPILEOPTS += -save-temps

DIRS	  = 

ESMF_TRACE_ESMFLIB = 
ifeq ($(ESMF_OS),Darwin)
  ESMF_TRACE_ESMFLIB = -lesmf
endif
ifeq  ($(ESMF_OS),MinGW)
  ESMF_TRACE_ESMFLIB = -lesmf
endif

tracelib_preload: preload.o preload_io.o preload_mpi.o wrappers.o wrappers_io.o wrappers_mpi.o
	$(ESMF_SL_LIBLINKER) $(ESMF_SL_LIBOPTS) -o $(ESMF_LDIR)/libesmftrace_preload.$(ESMF_SL_SUFFIX) $^ $(ESMF_CXXLINKOPTS) $(ESMF_CXXLINKPATHS) $(ESMF_CXXLINKRPATHS) $(ESMF_CXXLINKLIBS) $(ESMF_TRACE_ESMFLIB)

#	$(ESMF_SL_LIBLINKER) $(ESMF_SL_LIBOPTS) -o $(ESMF_LDIR)/libesmftrace_preload.$(ESMF_SL_SUFFIX) $^ $(ESMF_SL_LIBLIBS) $(ESMF_TRACE_ESMFLIB)

tracelib_static: wrappers_io.o wrappers_mpi.o wrappers.o
	$(ESMF_AR) $(ESMF_ARCREATEFLAGS) $(ESMF_LDIR)/libesmftrace_static.a $^
