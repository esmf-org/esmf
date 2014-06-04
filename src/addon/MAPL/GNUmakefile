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
# Top Makefile for ESMA systems.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMF_DIR
  $(error ESMF_DIR must be defined)
endif
BASEDIR  = $(ESMF_DIR)/src/addon/MAPL/
ESMADIR  = $(ESMF_DIR)/src/addon/MAPL/ESMA

#ALL: build_here
#
#LOCDIR   = src/addon/MAPL
#
#include $(ESMF_DIR)/makefile
#
#CLEANDIRS   =
#CLEANFILES  =
#CLOBBERDIRS =

# Compilation rules, flags, etc
# -----------------------------
#  include $(ESMF_DIR)/build_config/$(ESMF_OS).$(ESMF_COMPILER).$(ESMF_SITE)/build_rules.mk
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies


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


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------


NCEP_SHARED = $(wildcard NCEP_Shared)
     G5PERT = $(wildcard g5pert)
 GEOS_APERT = $(wildcard GEOSagcmPert_GridComp)
GMAO_SHARED = $(wildcard GMAO_Shared)

ASSERT  = $(wildcard $(ESMADIR)/src/Assert.pl)
MODINFO = $(wildcard $(ESMADIR)/src/g5_modules $(ESMADIR)/src/g5_modules.sh)

export ESMADIR BASEDIR ARCH SITE MAKEFLAGS ASSERT MODINFO

.PHONY: install local_install

# Handle stubs for NCEP implementarion of GOCART Stand alone
# ----------------------------------------------------------
  ifeq ($(GOCART_MODE),stub)

     ALLDIRS = Config GMAO_Shared/Chem_Base Components 

  else

     ALLDIRS = Config GMAO_mpeu MAPL_cfio MAPL_Base

  endif

SUBDIRS = $(wildcard $(ALLDIRS) )

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install doc

$(TARGETS):
	  for d in $(SUBDIRS); \
	  do			 \
	      echo Making $@ in $$d	;\
	      $(MAKE) -C $$d -e $@	;\
	      [ $$? -ne 0 ] && exit	;\
	  done || exit 0;

local_install:
	@echo Nothing to locally install here

pyinstall:
	$(MAKE) esma_install PYTHON_INSTALL=python_install

#                  --------------------
#                  User Defined Targets
#                  --------------------

distclean: esma_distclean
	-@$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] 

clean: esma_clean
	-@$(RM) *~ *.[aox] *.[Mm][Oo][Dd] 

binclean: 
	-$(RM) -r $(ESMADIR)/Config 
	-$(RM) -r $(ESMADIR)/$(ARCH)


realclean: distclean binclean


#         ---------------------------------------
#         Package Dependency for Parallel Install
#         ---------------------------------------

GCS := $(wildcard GEOSgcs_GridComp Components)

ifeq ($(NCEP_SHARED),$(null))
   NCEP_SHARED_INSTALL = 
else
   NCEP_SHARED_INSTALL = NCEP_Shared_install
endif

ifeq ($(G5PERT),$(null))
   G5PERT_INSTALL = 
else
   G5PERT_INSTALL = g5pert_install
endif

ifeq ($(GEOS_APERT),$(null))
   GEOS_APERT_INSTALL = 
else
   GEOS_APERT_INSTALL = GEOSagcmPert_GridComp_install
endif

ifeq ($(GMAO_SHARED),$(null))
   GMAO_SHARED_INSTALL = 
else
   GMAO_SHARED_INSTALL = GMAO_Shared_install
endif

                Config_install:
           NCEP_Shared_install: Config_install
                g5pert_install: GMAO_Shared_install
           GMAO_Shared_install: Config_install $(NCEP_SHARED_INSTALL)
 GEOSagcmPert_GridComp_install: GMAO_Shared_install
      GEOSgcs_GridComp_install: GMAO_Shared_install $(GEOS_APERT_INSTALL) $(G5PERT_INSTALL)
            Components_install: GMAO_Shared_install

          Applications_install: $(foreach gc, $(GCS), $(gc)_install ) $(GMAO_SHARED_INSTALL) $(NCEP_SHARED_INSTALL)

#                  --------------------
#                      Configuration
#                  --------------------

$(ESMADIR)/Config/ESMA_base.mk:
	$(MAKE) -C Config esma_install

$(ESMADIR)/Config/ESMA_arch.mk: ;

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros
#.
