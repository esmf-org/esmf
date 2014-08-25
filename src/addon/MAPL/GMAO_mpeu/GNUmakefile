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
# 3mar2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva  Standardization
# 19Feb2007  Stassi    Move from Makefile.dependency to $(DEPS);
#                      mods for build parallelization (pinstall)
# 22Jun2011 Guo        Fixed target "lib:" to be phony instead of real.
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


THIS = $(shell basename `pwd`)
LIB  = lib$(THIS).a
LIBeu  = $(subst mpeu,eu,$(LIB))

esma_install install: $(LIB) $(LIBeu)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) -p *.a    $(ESMALIB)
	$(CP) -p *.[Hh] $(ESMAINC)/$(THIS)
	$(CP) -p *.[Mm][Oo][Dd] $(ESMAINC)/$(THIS)

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd] 
	$(MAKE) -C mpi0 clean

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] 
	$(MAKE) -C mpi0 distclean

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


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




#                  --------------------
#                  User Defined Targets
#                  --------------------

SRCS_C		= get_zeits.c

SRCS_F		= m_mpif.F m_random.F

SRCS_f90        = m_FileResolv.f90

SRCS_F90	= m_date.F90			\
		  m_datetime.F90		\
		  m_IndexBin_char.F90		\
		  m_IndexBin_integer.F90	\
		  m_IndexBin_logical.F90	\
		  m_List.F90			\
		  m_MergeSorts.F90		\
		  m_Filename.F90		\
		  m_Group.F90			\
		  m_Permuter.F90		\
		  m_SortingTools.F90		\
		  m_String.F90			\
		  m_StrTemplate.F90		\
		  m_chars.F90			\
		  m_die.F90			\
		  m_dropdead.F90		\
		  m_flow.F90			\
		  m_inpak90.F90			\
		  m_intkinds.F90		\
		  m_ioutil.F90			\
		  m_mall.F90			\
		  m_mergedList.F90		\
		  m_mpif90.F90			\
		  m_mpout.F90			\
                  m_parDOT.F90                  \
		  m_rankMerge.F90		\
		  m_realkinds.F90		\
		  m_showDistrib.F90		\
		  m_stdio.F90			\
		  m_zeit.F90			\
		  m_ior4.F90			\
		  m_rGrADS.F90			\
		  m_wGrADS.F90			\
		  m_GrADS.F90			\
                  m_fpe.F90

SRCS_ALL	= $(SRCS_f)		\
		  $(SRCS_F)		\
		  $(SRCS_f90)		\
		  $(SRCS_F90)		\
		  $(SRCS_C)

OBJS_ALL	= $(SRCS_f:.f=.o)	\
		  $(SRCS_F:.F=.o)	\
		  $(SRCS_f90:.f90=.o)	\
		  $(SRCS_F90:.F90=.o)	\
		  $(SRCS_C:.c=.o)

DEPS    	= $(SRCS_f:.f=.d)	\
		  $(SRCS_F:.F=.d)	\
		  $(SRCS_f90:.f90=.d)	\
		  $(SRCS_F90:.F90=.d)

MUPDATE		= sh UCupdate.sh

MODS_OBJ        = m_chars.o		\
		  m_date.o		\
		  m_datetime.o		\
		  m_die.o		\
		  m_Filename.o		\
                  m_FileResolv.o        \
		  m_flow.o		\
		  m_inpak90.o 		\
		  m_ioutil.o 		\
		  m_List.o 		\
		  m_MergeSorts.o	\
		  m_mall.o		\
		  m_mpif90.o		\
		  m_mpout.o		\
		  m_Permuter.o		\
		  m_parDOT.o		\
		  m_realkinds.o		\
		  m_SortingTools.o	\
		  m_String.o		\
		  m_StrTemplate.o	\
		  m_showDistrib.o	\
		  m_stdio.o		\
		  m_zeit.o              \
		  assert.H


USER_FINCS  += $(I). $(I)$(INC_MPI)
USER_CINCS  += $(I). $(I)$(INC_MPI)
USER_FFLAGS += $(BIG_ENDIAN) $(M). 

MOD_DIRS = . $(INC_MPI)
FREAL = $(FREAL4)

$(LIBeu): $(OBJS_ALL) mpi0/libmpi0.a
	$(RM) $@
	cp -p mpi0/libmpi0.a $@
	@ echo $(AR) cq $@ $(OBJS_ALL)
	@ $(AR) cq $@ $(OBJS_ALL)

mpi0/libmpi0.a:
	$(MAKE) -C mpi0 libmpi0.a

vpath % $(MOD_DIRS)

lib:	$(LIBeu) $(LIB)

$(LIB):	$(OBJS_ALL)
	$(RM) $@
	@ echo $(AR) cq $@ $(OBJS_ALL)
	@ $(AR) cq $@ $(OBJS_ALL)

#                  --------------------
#                      Dependencies
#                  --------------------

#include Makefile.dependency

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif


  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
