#
# recursive makefile for ESMA.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif


# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help :
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


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

ALLDIRS = arpack pnagpack \
          GMAO_mpeu GMAO_pilgrim GMAO_mfhdf3 \
          GMAO_gfio GMAO_gfioeos GMAO_perllib \
          GMAO_ods  MAPL_cfio \
          MAPL_Base GEOS_Shared Chem_Base Chem_Shared\
          GMAO_etc  GMAO_hermes GMAO_transf GMAO_psas GMAO_gems \
          GFDL_fms LANL_Shared \
          GMAO_iret GEOS_Util GMAO_pyobs GMAO_radmon

SUBDIRS = $(wildcard $(ALLDIRS) )

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          help install doc clean distclean

export ESMADIR BASEDIR ARCH SITE

$(TARGETS): 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( cd $$d				;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done


local_esma_install local_install: $(LIB)
	@echo No local install in here

#              ------------------------------------------
#              Package Dependencies for Parallel Install
#              ------------------------------------------

ifeq ($(wildcard GMAO_mfhdf3), $(null))
   GMAO_MFHDF3 =
else
   GMAO_MFHDF3 = GMAO_mfhdf3_install
endif

ifeq ($(wildcard GMAO_mpeu), $(null))
   GMAO_MPEU =
else
   GMAO_MPEU = GMAO_mpeu_install
endif

ifeq ($(wildcard GMAO_pilgrim), $(null))
   GMAO_PILGRIM =
else
   GMAO_PILGRIM = GMAO_pilgrim_install
endif

ifeq ($(wildcard MAPL_cfio), $(null))
   MAPL_CFIO =
else
   MAPL_CFIO = MAPL_cfio_install
endif

Chem_Base_install   : MAPL_Base_install   \
                      GEOS_Shared_install \
                      $(GMAO_MPEU)        \
                      GMAO_gfio_install   \
                      $(GMAO_MFHDF3)
Chem_Shared_install : Chem_Base_install   \
                      MAPL_Base_install   \
                      GEOS_Shared_install \
                      GMAO_gfio_install   \
                      $(GMAO_MPEU)        \
                      $(GMAO_PILGRIM)

MAPL_Base_install   : $(MAPL_CFIO)
GEOS_Shared_install : MAPL_Base_install   \
                      $(MAPL_CFIO)
GEOS_Util_install   : GMAO_gfio_install   \
                      MAPL_Base_install   \
                      GMAO_hermes_install \
                      $(MAPL_CFIO)        \
                      $(GMAO_MFHDF3)
MAPL_cfio_install   : $(GMAO_MPEU)        \
                      $(GMAO_MFHDF3)
GMAO_etc_install    : $(GMAO_MPEU)
GMAO_gfio_install   : $(GMAO_MFHDF3)      \
                      $(GMAO_MPEU)
GMAO_hermes_install : GMAO_etc_install    \
                      $(GMAO_MPEU)        \
                      $(MAPL_CFIO)        \
                      GMAO_gfio_install   \
                      MAPL_Base_install   \
                      GEOS_Shared_install
GMAO_iret_install   : GMAO_hermes_install \
                      $(GMAO_MPEU)        \
                      Chem_Base_install
GMAO_ods_install    : $(GMAO_MPEU)        \
                      $(GMAO_PILGRIM)
GMAO_psas_install   : $(GMAO_MPEU)        \
                      GMAO_ods_install
GMAO_transf_install : GMAO_hermes_install \
                      $(GMAO_MPEU)        \
                      GMAO_hermes_install
LANL_Shared_install : MAPL_Base_install

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
