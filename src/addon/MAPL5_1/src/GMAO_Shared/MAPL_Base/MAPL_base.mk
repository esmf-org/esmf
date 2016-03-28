#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 09Jun2003  da Silva  First crack.
# 23Aug2006  da Silva  Adapt to build both MAPL/GEOS_Base.
# 07Sep2006  da Silva  Changed code base from GEOS_*.F90 to MAPL_*.F90
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies

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
	@echo "         SITE = $(SITE)"
	@echo "         CFIO = $(CFIO)"

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

esma_install install: $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAINC)/$(THIS)
	$(CP) -p ../mapl_*.pl      $(ESMABIN)
	$(CP) -p *.a            $(ESMALIB)
	$(CP) -p *.mod          $(ESMAINC)/$(THIS)
	$(CP) -p $(IINCS)       $(ESMAINC)/$(THIS)

esma_clean clean:
	-$(RM) *~ *.[aox] *.mod *.x 

#                  --------------------
#                  User Defined Targets
#                  --------------------

INCS :=  MAPL_ErrLog.h            MAPL_ErrLogMain.h      MAPL_Generic.h

CODE :=  allgather.code           bcast.code             recv.code      \
         allgatherv.code          gather.code            scatter.code\
         arraygather.code         sun.code               send.code\
         arrayscatter.code        read_parallel.code     sendrecv.code


SRCS :=  ESMFL_Mod.P90            MAPL_HeapMod.F90       MAPL_VarSpecMod.F90\
         MAPL_Base.F90            MAPL_InterpMod.F90     hinterp.F\
         MAPL_CFIO.F90            MAPL_IO.P90            MAPL_HorzTransform.F90\
         MAPL_Comms.P90           MAPL_LocStreamMod.F90  MAPL_Mod.F90\
         MAPL_Constants.F90       MAPL_Profiler.F90      sort.c\
         MAPL_GenericCplComp.F90  MAPL_Sort.F90\
         MAPL_Generic.F90         MAPL_sun_uc.P90        MAPL_SatVapor.F90\
         MAPL_HistoryGridComp.F90 MAPL_Cap.F90

ifeq ($(THIS),GEOS_Base)
   INCS := $(patsubst MAPL_%, GEOS_%,$(INCS) )
   CODE := $(patsubst %.H, %.code,$(CODE) )
#   SRCS := $(patsubst MAPL_%, GEOS_%,$(SRCS) )
#   SRCS := $(patsubst ESMFL_%, GEOS_ESMFL_%,$(SRCS) )
   SRCS = GEOS_Constants.F90
   TESTS = 
   IINCS = GEOS_*.h
else
   TESTS = utCFIO_Bundle.x utCFIO_Array.x utCFIO_Nbits.x 
   IINCS = ../MAPL_*.h 
endif

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . .. $(INC_MPI) /usr/include
MOD_DIRS = $(INC_DIRS) $(INC_ESMF) $(INC_CFIO) $(ESMAINC)/GMAO_mpeu

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FFLAGS = $(D)MAPL

FREAL = $(FREAL4) # for now, require 32 bit reals (R4)

vpath % $(MOD_DIRS) ../tests

$(LIB) lib : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

%.x : $(LIB) %.o
	$(FC) $(LDFLAGS) -o $@ $*.o $(LIB) \
              $(LIB_ESMF) $(LIB_CFIO) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

test: $(TESTS)
	  @ argv="$(TESTS)" ;\
	  for test in $$argv; do			 \
	    ( echo " "; echo "---- Starting test $$test"; \
              mpirun -np 4 $$test ) \
	  done


#                  ----------------------------
#                   MAPL Files from GEOS Files
#                  ----------------------------

ifeq ($(THIS),GEOS_Base)

GEOS_%.h : ../MAPL_%.h
	@sed -e 's/[Mm][Aa][Pp][Ll]_/GEOS_/g' $< > $@

%.code : ../%.H
	@sed -e 's/[Mm][Aa][Pp][Ll]_/GEOS_/g' $< > $@

GEOS_%.F : ../MAPL_%.F
	@sed -e 's/[Mm][Aa][Pp][Ll]_/GEOS_/g' $< > $@

GEOS_Mod.F90 : ../MAPL_Mod.F90
	@sed -e 's/use MAPL_/use GEOS_/g' $< > $@

GEOS_%.F90 : ../MAPL_%.F90
	@sed -e 's/[Mm][Aa][Pp][Ll]_/GEOS_/g' $< > $@

GEOS_%.P90 : ../MAPL_%.P90
	@sed -e 's/[Mm][Aa][Pp][Ll]_/GEOS_/g' -e 's/\.H/\.code/g' $< > $@

GEOS_ESMFL_Mod.F90 : ../ESMFL_Mod.P90
	@sed -e 's/[Mm][Aa][Pp][Ll]_/GEOS_/g' $< > $@

DEBRIS := GEOS_*.[fFhP]* *.H

$(DEPS) : $(SRCS) $(INCS) $(CODE)

endif

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.mod *.x $(DEBRIS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

#.

