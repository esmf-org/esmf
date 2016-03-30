#! /bin/make

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../../
endif

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a
LIB_STUBS  = lib$(THIS)_stubs.a

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk



LIB_SYS    =  -lpthread -lrt -lstd++ #-lefence

.SUFFIXES: .F90


SRCS = $(wildcard *.F90)
OBJS = $(SRCS:.F90=.o) 
BIN = $(SRCS:.F90=.x) 

INC_DIRS = . $(INC_MAPL_BASE) $(INC_ESMF)
MOD_DIRS = . $(INC_DIRS) $(INC_CFIO) $(INC_MPEU) 

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir))
         
LIB_COMP = -L$(LIB_MAPL) -lMAPL_Base -lMAPL_Base_stubs -lMAPL_cfio_r4 -lGMAO_mpeu


LIB_SDF = $(LIB_NETCDF)	   

$(BIN) bin: $(OBJS)
	$(LD) -o $(BIN) $(LDFLAGS) $(OBJS) $(LIB_COMP) $(LIB_ESMF) $(LIB_MPI) $(LIB_SDF) $(LIB_SCI) $(LIB_SYS) 

#                  --------------------
#                      Dependencies
#                  --------------------
.F90.o:
	$(FC) -c $(FFLAGS) $(FINCS) $<

utCFIO_Bundle.o: utCFIO_Bundle.F90

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
