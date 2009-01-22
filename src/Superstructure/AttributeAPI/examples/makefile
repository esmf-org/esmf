# $Id: makefile,v 1.3 2009/01/22 17:38:20 rokuingh Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/AttributeAPI/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_AttributeEx \
		    $(ESMF_EXDIR)/ESMF_AttributePackageEx  \
                    $(ESMF_EXDIR)/ESMF_AttributeUpdateEx

EXAMPLES_RUN      = run_ESMF_AttributeEx \
		    run_ESMF_AttributePackageEx \
                    run_ESMF_AttributeUpdateEx

EXAMPLES_RUN_UNI  = run_ESMF_AttributeEx_uni \
                    run_ESMF_AttributePackageEx_uni \
                    run_ESMF_AttributeUpdateEx_uni

#ESMF_AttributeUpdateEx.o : user_model.o

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_Attribute
#

# --- ESMF_AttributeEx

run_ESMF_AttributeEx:
	$(MAKE) EXNAME=Attribute NP=4 exfrun

run_ESMF_AttributeEx_uni:
	$(MAKE) EXNAME=Attribute NP=1 exfrun

# --- ESMF_AttributePackageEx

run_ESMF_AttributePackageEx:
	$(MAKE) EXNAME=AttributePackage NP=4 exfrun

run_ESMF_AttributePackageEx_uni:
	$(MAKE) EXNAME=AttributePackage NP=1 exfrun

# --- ESMF_AttributeUpdateEx

run_ESMF_AttributeUpdateEx:
	$(MAKE) EXNAME=AttributeUpdate NP=4 exfrun

run_ESMF_AttributeUpdateEx_uni:
	$(MAKE) EXNAME=AttributeUpdate NP=1 exfrun

