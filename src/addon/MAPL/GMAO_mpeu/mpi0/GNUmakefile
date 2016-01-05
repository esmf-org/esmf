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


THIS = mpi0
LIB  = lib$(THIS).a

#FC = mpif90 

esma_install install: $(LIB)
	$(MKDIR) $(ESMALIB) 
#	$(MKDIR) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) -p *.a    $(ESMALIB)
#	$(CP) -p *.h $(ESMAINC)/$(THIS)
#	$(CP) -p *.[Mm]{Oo][Dd] $(ESMAINC)/$(THIS)

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd] 

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

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
SRCS_F90	= m_SHMEM.F90                   \
		  mpi_abort.F90			\
		  mpi_comm_dup.F90		\
		  mpi_comm_free.F90		\
		  mpi_comm_rank.F90		\
		  mpi_comm_size.F90		\
		  mpi_error_string.F90		\
		  mpi_finalize.F90		\
		  mpi_init.F90			\
		  mpi_initialized.F90		\
		  mpi_isend.F90			\
		  mpi_send.F90			\
		  mpi_sendrecv.F90		\
		  mpi_recv.F90			\
		  mpi_irecv.F90			\
		  mpi_waitall.F90               \
		  mpi_wtick.F90			\
		  mpi_wtime.F90			\
		  mpi_barrier.F90		\
		  mpi_bcast.F90			\
		  mpi_gather.F90		\
		  mpi_gatherv.F90		\
		  mpi_scatter.F90		\
		  mpi_scatterv.F90		\
		  mpi_allgather.F90		\
		  mpi_allgatherv.F90		\
		  mpi_alltoall.F90		\
		  mpi_alltoallv.F90		\
		  mpi_reduce.F90		\
		  mpi_allreduce.F90		\
		  mpi_reduce_scatter.F90	\
		  mpi0_copy.F90			\
		  mpi_zzz.F90                   \
                  mpi_wait.F90                  \
                  mpi_comm_split.F90

SRCS_F		= m_mpi0.F 

SRCS_H		= Const_MPItype.H

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

FOPT = $(FOPT3)
COPT = $(COPT3)

USER_FINCS  = $(I). $(I)$(INC_MPI)
USER_CINCS  = $(I). $(I)$(INC_MPI)
MOD_DIRS = . $(INC_MPI)
USER_FFLAGS = $(BIG_ENDIAN) $(M). $(M)../jic
USER_FDEFS  = $(D)LANGUAGE_FORTRAN
FREAL =

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS_ALL)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS_ALL)


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
