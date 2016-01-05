# $Id$

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/AttributeAPI/examples

.NOTPARALLEL:
EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_AttributeEx \
		    $(ESMF_EXDIR)/ESMF_AttributePackageEx \
		    $(ESMF_EXDIR)/ESMF_AttributeCustPackEx \
		    $(ESMF_EXDIR)/ESMF_AttributeUpdateEx \
		    $(ESMF_EXDIR)/ESMF_AttributeCIMEx \
		    $(ESMF_EXDIR)/ESMF_AttributeInternalInfoEx \
		    $(ESMF_EXDIR)/ESMF_AttReadCustCplCompEx \
		    $(ESMF_EXDIR)/ESMF_AttReadGridCompEx \
		    $(ESMF_EXDIR)/ESMF_AttReadFieldEx \

ESMF_EXAMPLE_AttributeUpdate_OBJS = ESMF_AttributeUpdateMod.o

EXAMPLES_RUN      = run_ESMF_AttributeEx \
		    run_ESMF_AttributePackageEx \
		    run_ESMF_AttributeCustPackEx \
		    run_ESMF_AttributeUpdateEx \
		    run_ESMF_AttributeCIMEx \
		    run_ESMF_AttributeInternalInfoEx \
		    run_ESMF_AttReadCustCplCompEx \
		    run_ESMF_AttReadGridCompEx \
		    run_ESMF_AttReadFieldEx \

EXAMPLES_RUN_UNI  = run_ESMF_AttributeEx_uni \
		    run_ESMF_AttributePackageEx_uni \
		    run_ESMF_AttributeCustPackEx_uni \
		    run_ESMF_AttributeCIMEx_uni \
		    run_ESMF_AttributeInternalInfoEx_uni \
		    run_ESMF_AttReadCustCplCompEx_uni \
		    run_ESMF_AttReadGridCompEx_uni \
		    run_ESMF_AttReadFieldEx_uni \

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

# --- ESMF_AttributeCustPackEx

run_ESMF_AttributeCustPackEx:
	$(MAKE) EXNAME=AttributeCustPack NP=4 exfrun

run_ESMF_AttributeCustPackEx_uni:
	$(MAKE) EXNAME=AttributeCustPack NP=1 exfrun

# --- ESMF_AttributeUpdateEx

run_ESMF_AttributeUpdateEx:
	$(MAKE) EXNAME=AttributeUpdate NP=4 exfrun

# --- ESMF_AttributeCIMEx

run_ESMF_AttributeCIMEx:
	$(MAKE) EXNAME=AttributeCIM NP=4 exfrun

run_ESMF_AttributeCIMEx_uni:
	$(MAKE) EXNAME=AttributeCIM NP=1 exfrun

# --- ESMF_AttributeInternalInfoEx

run_ESMF_AttributeInternalInfoEx:
	$(MAKE) EXNAME=AttributeInternalInfo NP=4 exfrun

run_ESMF_AttributeInternalInfoEx_uni:
	$(MAKE) EXNAME=AttributeInternalInfo NP=1 exfrun

# --- ESMF_AttReadCustCplCompEx

run_ESMF_AttReadCustCplCompEx:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/custom_cplcomp.xml $(ESMF_EXDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/custom_cplcomp.xsd $(ESMF_EXDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadCustCplComp NP=4 exfrun

run_ESMF_AttReadCustCplCompEx_uni:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/custom_cplcomp.xml $(ESMF_EXDIR)
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/custom_cplcomp.xsd $(ESMF_EXDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadCustCplComp NP=1 exfrun

# --- ESMF_AttReadGridCompEx

run_ESMF_AttReadGridCompEx:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_EXDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadGridComp NP=4 exfrun

run_ESMF_AttReadGridCompEx_uni:
	cp -f $(ESMF_DIR)/src/Superstructure/Component/etc/esmf_gridcomp.xml $(ESMF_EXDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadGridComp NP=1 exfrun

# --- ESMF_AttReadFieldEx

run_ESMF_AttReadFieldEx:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_EXDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadField NP=4 exfrun

run_ESMF_AttReadFieldEx_uni:
	cp -f $(ESMF_DIR)/src/Infrastructure/Field/etc/esmf_field.xml $(ESMF_EXDIR)
	$(ESMF_DIR)/scripts/copy_xsd $(ESMF_EXDIR)
	$(MAKE) EXNAME=AttReadField NP=1 exfrun
