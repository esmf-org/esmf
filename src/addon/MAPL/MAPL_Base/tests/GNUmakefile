#! /bin/make
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

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMF_DIR
  $(error ESMF_DIR must be defined)
endif
BASEDIR  = $(ESMF_DIR)/src/addon/MAPL/
ESMADIR  = $(ESMF_DIR)/src/addon/MAPL/ESMA

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

INC_DIRS = . $(INC_MAPL_BASE)
MOD_DIRS = . $(INC_DIRS) $(INC_CFIO) $(INC_MPEU) 

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) $(INC_ESMF)
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
