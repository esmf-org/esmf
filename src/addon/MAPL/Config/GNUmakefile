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
# Makefile for ESMA build configuration system
#

# Generic Base/Arch + User Defined Fragments
# ------------------------------------------
  ESMA_BASE = ESMA_base.mk $(wildcard $(HOME)/.esma_base.mk) 
  ESMA_ARCH = ESMA_arch.mk $(wildcard $(HOME)/.esma_arch.mk) 
  ESMA_POST = ESMA_post.mk $(wildcard $(HOME)/.esma_post.mk) 

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMA_BASE)            # Generic stuff
  include $(ESMA_ARCH)            # System dependencies

# Make sure ESMADIR/BASEDIR are defined
# -------------------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif

  BINS    = # f90aib.x f90split.x
  SCRIPTS = mklpath.pl endiantest.pl OScheck.pl esma_timer.sh esma_tgraph.pl

# User can provide their own assert script
# ----------------------------------------
  ASSERT_MK = 
  ifdef ASSERT
        RC = $(shell $(ASSERT))
        ifeq ($(RC), 0)
              $(warning Enabling build environment check)
	      ASSERT_MK = Assert.mk
        else 
           ifeq ($(RC), -1)
             $(warning - Node not know, disabling build environment check)
           else 
             $(error Please correct your build environment and try again)
           endif
        endif
  endif

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

esma_install install : fdp.o $(BINS)
	$(MKDIR) $(ESMABIN) $(ESMALIB) $(ESMAINC) \
		 $(ESMAETC) $(ESMADOC) $(ESMACFG)

	@if [ "$(ESMACFG)" != "$(ESMADIR)/Config" ]; then (\
	   $(RM) -rf $(ESMADIR)/Config;  \
	   cd $(ESMADIR); $(LN) -s $(ARCH)/Config . \
	) fi

	$(CAT) $(ESMA_BASE) > $(ESMACFG)/ESMA_base.mk
	$(CAT) $(ESMA_ARCH) > $(ESMACFG)/ESMA_arch.mk
	$(CAT) $(ESMA_POST) > $(ESMACFG)/ESMA_post.mk
	$(CAT) GMAO_base.mk $(ASSERT_MK) > $(ESMACFG)/GMAO_base.mk
	echo "I = $(I)"              > $(ESMACFG)/NCEP_base.mk
	echo "LIB_SDF = $(LIB_SDF)" >> $(ESMACFG)/NCEP_base.mk
	$(CAT) NCEP_base.mk         >> $(ESMACFG)/NCEP_base.mk

	echo "BASEDIR: $(BASEDIR)" > $(ESMAETC)/BASEDIR.rc

	$(CP) -p protex $(MODINFO) $(BINS) $(SCRIPTS) $(ESMABIN)

ifdef ASSERT
	$(CP) $(ASSERT) $(ESMABIN)/Assert
endif
	./install_fdp ./fdp $(FDP)
	./CVSTAG_install.pl $(ESMAETC)
	./OScheck.pl -q -cp $(ESMAETC)

esma_clean clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"

FOPT =

# If ESMA_TIMER is enabled, fdp.o can't use the "standard"
# .f90.o rule since it gets compiled before esma_timer.sh is
# copied into ESMABIN. Easiest solution, just undefine ESMA_TIMER
# locally.

ESMA_TIMER =

%.x : %.F90
	$(FC) -o $@ $(F90FLAGS) $<

  include $(ESMA_POST)

#.

