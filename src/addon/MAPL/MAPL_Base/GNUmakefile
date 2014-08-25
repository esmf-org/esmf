# +-======-+ 
#  Copyright (c) 2003-2007 United States Government as represented by 
#  the Admistrator of the National Aeronautics and Space Administration.  
#  All Rights Reserved.
#  
#  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
#  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
#  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
#  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
#  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
#  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
#  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
#  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
#  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
#  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
#  
#  Government Agency: National Aeronautics and Space Administration
#  Government Agency Original Software Designation: GSC-15354-1
#  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
#  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
#  Government Agency Point of Contact for Original Software:  
#  			Dale Hithon, SRA Assistant, (301) 286-2691
#  
# +-======-+ 
#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 20oct2004  da Silva  First ESMA version.
# 23Aug2006  da Silva  Driver for making either MAPL or GEOS_Base.
# 26Aug2008  Stassi    No longer builds GEOS_Base; merged with MAPL_base.mk
# 27Oct2008  da SIlva  Some clean up for ESMF v3
# 09Feb2010  Todling   Another stub-lib was added (stub2). Saidly, it cannot be 
#                      folded w/ the exiting stub-lib
# 17Jul2012  Trayanov  Removed stub2
# 16Aug2012  Purnendu  Added subdir TeX
# -------------------------------------------------------------------------


# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../..
endif

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a
LIB_STUBS  = lib$(THIS)_stubs.a

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE) "
	@echo "         THIS = $(THIS) "

#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

ALLDIRS = TeX

SUBDIRS = $(wildcard $(ALLDIRS))

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc 

export ESMADIR BASEDIR ARCH SITE FREAL

$(TARGETS): 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( cd $$d				;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done
	$(MAKE) local_$@

local_esma_install local_install: $(LIB) $(LIB_STUBS)
	$(MKDIR) $(ESMALIB) $(ESMAINC)/$(THIS)
	$(CP) -pr *.a $(wildcard Python)   $(ESMALIB)
	$(CP) -p *.[Mm][Oo][Dd] $(IINCS)   $(ESMAINC)/$(THIS)
	$(CP) -p *.pl                      $(ESMABIN) 


local_esma_clean local_clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *___.f90

local_esma_distclean local_distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *___.f90

local_esma_doc local_doc:
	@echo "Target $@ not implemented in `pwd`"

#                  --------------------
#                  User Defined Targets
#                  --------------------

INCS :=  MAPL_ErrLog.h            MAPL_ErrLogMain.h      MAPL_Generic.h \
         MAPL_Exceptions.h

CODE :=  allgather.code           bcast.code             recv.code      \
         allgatherv.code          gather.code            scatter.code\
         arraygather.code         sun.code               send.code\
         arrayscatter.code        read_parallel.code     sendrecv.code\
         allreducemin.code        allreducemax.code      allreducesum.code

SRCS :=  ESMFL_Mod.P90            MAPL_HeapMod.F90       MAPL_VarSpecMod.F90\
         MAPL_Base.F90            MAPL_InterpMod.F90     hinterp.F\
         MAPL_CFIO.F90            MAPL_IO.P90            MAPL_HorzTransform.F90\
         MAPL_Comms.P90           MAPL_LocStreamMod.F90  MAPL_Mod.F90\
         MAPL_Constants.F90       MAPL_Profiler.F90      sort.c\
         MAPL_GenericCplComp.F90  MAPL_Sort.F90\
         MAPL_Generic.F90         MAPL_sun_uc.P90        MAPL_SatVapor.F90\
         MAPL_HistoryGridComp.F90 MAPL_Cap.F90           hash.c\
         c_mapl_locstream_F.c\
         memuse.c                 MAPL_MemUtils.F90      MAPL_Hash.F90 \
         CubeToLatLon.F90         MAPL_MaxMinMod.F90     MAPL_SimpleBundleMod.F90 \
         MAPL_OrbGridCompMod.F90  MAPL_NominalOrbitsMod.F90 MAPL_ExtDataGridCompMod.F90 \
         MAPL_NewArthParser.F90   MAPL_ShmemMod.F90      MAPL_LoadBalance.F90 MAPL_CFIOServer.F90

SRC_STUBS :=  MAPL_stubs.F90

# Hack to allow runs on jaguar
# FOPT = $(FOPT0) 

TESTS = utCFIO_Bundle.x utCFIO_Array.x utCFIO_Nbits.x 
IINCS = $(wildcard MAPL_*.h)

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS) $(SRC_STUBS)))

OBJ_STUBS := $(addsuffix .o, $(basename $(SRC_STUBS)))

INC_DIRS = . $(INC_MPI) /usr/include $(INC_SDF)
MOD_DIRS = $(INC_DIRS) $(INC_CFIO) $(INC_MPEU)

USER_FINCS = $(foreach dir,$(INC_DIRS),$(I)$(dir)) $(foreach dir,$(INC_ESMF),$(dir))
USER_FMODS = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FDEFS = $(D)MAPL $(D)DO_COMMAS $(D)TWO_SIDED_COMM

FREAL = $(FREAL4) # for now, require 32 bit reals (R4)
THIS_CFIO = MAPL_cfio_r4

# this a somewhat sloppy way to bypass Cubed Sphere dependences:
# we check our sandbox if FVdycoreCubed is in the sandbox. 
# If yes, we compile with Cubed Sphere support 
ifneq ( $(wildcard $(ESMADIR)/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSsuperdyn_GridComp/FVdycoreCubed_GridComp ), $(null) ) 
   USER_FFLAGS += -DUSE_CUBEDSPHERE
endif
#
# more of the same
#
ifneq ( $(wildcard $(ESMADIR)/src/GEOSagcmPert_GridComp ), $(null) ) 
   USER_FFLAGS += -DUSE_CUBEDSPHERE
endif

ifdef GPU_TARGET
  INC_DIRS += $(INC_PGI) # Needed to pick up CUDA libraries
endif

vpath % $(MOD_DIRS) tests TeX

$(LIB) lib : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

$(LIB_STUBS) lib : $(DEPS) $(OBJ_STUBS)
	$(AR) $(AR_FLAGS) $(LIB_STUBS) $(OBJ_STUBS)

%.x : $(LIB) %.o
	$(FC) $(LDFLAGS) -o $@ $*.o $(LIB) $(LIB_STUBS) \
              $(LIB_ESMF) $(LIB_CFIO) $(LIB_MPEU) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

test: $(TESTS)
	  @ argv="$(TESTS)" ;\
	  for test in $$argv; do			 \
	    ( echo " "; echo "---- Starting test $$test"; \
              mpirun -np 4 $$test ) \
	  done

# #                  --------------------
# #                     Documentation
# #                  --------------------

# PDF_DOCS = MAPL_UsersGuide.pdf  \
#            MAPL_Base.pdf       \
#            MAPL_Cap.pdf        \
#            MAPL_CFIO.pdf       \
#            MAPL_Exceptions.pdf \
#            MAPL_HistoryGridComp.pdf       

# esma_doc doc: $(PDF_DOCS)

# UG_SRCS = MAPL_Generic.F90 MAPL_GenericCplComp.F90  \
#           MAPL_sun_uc.P90 MAPL_SatVapor.F90

# MAPL_UsersGuide.tex: $(UG_SRCS)
# 	$(PROTEX) -g -b -f $(UG_SRCS) > MAPL_UsersGuide___.tex
# 	$(CAT) TeX/MAPL_Intro.tex MAPL_UsersGuide___.tex > MAPL_UsersGuide.tex

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
