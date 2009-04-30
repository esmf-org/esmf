# $Id: makefile,v 1.10 2009/04/30 05:01:01 eschwab Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/AttributeAPI/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_AttributeEx \
		    $(ESMF_EXDIR)/ESMF_AttributePackageEx \
		    $(ESMF_EXDIR)/ESMF_AttributeUpdateEx \
		    $(ESMF_EXDIR)/ESMF_AttReadGridCompEx \
		    $(ESMF_EXDIR)/ESMF_AttReadFieldEx \
                    $(ESMF_EXDIR)/ESMF_AttReadGridEx

ESMF_EXAMPLE_AttributeUpdate_OBJS = ESMF_AttributeUpdateMod.o

EXAMPLES_RUN      = run_ESMF_AttributeEx \
		    run_ESMF_AttributePackageEx \
		    run_ESMF_AttributeUpdateEx \
		    run_ESMF_AttReadGridCompEx \
		    run_ESMF_AttReadFieldEx \
                    run_ESMF_AttReadGridEx

EXAMPLES_RUN_UNI  = run_ESMF_AttributeEx_uni \
		    run_ESMF_AttributePackageEx_uni \
		    run_ESMF_AttReadGridCompEx_uni \
		    run_ESMF_AttReadFieldEx_uni \
                    run_ESMF_AttReadGridEx_uni

ESMF_AttributeUpdateEx.o : $(ESMF_EXAMPLE_AttributeUpdate_OBJS)

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

# --- ESMF_AttReadGridCompEx

run_ESMF_AttReadGridCompEx:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadGridComp NP=4 exfrun

run_ESMF_AttReadGridCompEx_uni:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadGridComp NP=1 exfrun

# --- ESMF_AttReadFieldEx

run_ESMF_AttReadFieldEx:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadField NP=4 exfrun

run_ESMF_AttReadFieldEx_uni:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadField NP=1 exfrun

# --- ESMF_AttReadGridEx

run_ESMF_AttReadGridEx:
	cp -f $(ESMF_DIR)/src/Infrastructure/Grid/etc/esmf_grid.xml $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadGrid NP=4 exfrun

run_ESMF_AttReadGridEx_uni:
	cp -f $(ESMF_DIR)/src/Infrastructure/Grid/etc/esmf_grid.xml $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadGrid NP=1 exfrun
