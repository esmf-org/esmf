# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/Field/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_FieldCreateEx \
                 $(ESMF_EXDIR)/ESMF_FieldEx \
                 $(ESMF_EXDIR)/ESMF_FieldRegridEx \
                 $(ESMF_EXDIR)/ESMF_FieldRegridMaskEx \
                 $(ESMF_EXDIR)/ESMF_FieldSphereRegridEx \
                 $(ESMF_EXDIR)/ESMF_FieldMeshRegridEx \
                 $(ESMF_EXDIR)/ESMF_FieldRepDimEx \
                 $(ESMF_EXDIR)/ESMF_FieldArbGridEx \
                 $(ESMF_EXDIR)/ESMF_FieldCommEx \
                 $(ESMF_EXDIR)/ESMF_FieldRedistEx \
                 $(ESMF_EXDIR)/ESMF_FieldSMMEx \
                 $(ESMF_EXDIR)/ESMF_FieldHaloEx

EXAMPLES_RUN = run_ESMF_FieldCreateEx \
               run_ESMF_FieldEx \
               run_ESMF_FieldRegridEx \
               run_ESMF_FieldRegridMaskEx \
               run_ESMF_FieldSphereRegridEx \
               run_ESMF_FieldMeshRegridEx \
               run_ESMF_FieldRepDimEx \
               run_ESMF_FieldArbGridEx \
               run_ESMF_FieldCommEx \
               run_ESMF_FieldRedistEx \
               run_ESMF_FieldSMMEx \
               run_ESMF_FieldHaloEx

EXAMPLES_RUN_UNI = run_ESMF_FieldArbGridEx_uni

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = foo.nc $(EXAMPLES_BUILD) 0 1 2 3
CLOBBERDIRS =

#
# ESMF_FieldEx
#
run_ESMF_FieldEx:
	$(MAKE) EXNAME=Field NP=4 exfrun


#
# ESMF_FieldCreateEx
#
run_ESMF_FieldCreateEx:
	$(MAKE) EXNAME=FieldCreate NP=4 exfrun

#
# ESMF_FieldRegridEx
#
run_ESMF_FieldRegridEx:
	$(MAKE) EXNAME=FieldRegrid NP=4 exfrun

#
# ESMF_FieldRegridMaskEx
#
run_ESMF_FieldRegridMaskEx:
	$(MAKE) EXNAME=FieldRegridMask NP=4 exfrun


#
# ESMF_FieldSphereRegridEx
#
run_ESMF_FieldSphereRegridEx:
	$(MAKE) EXNAME=FieldSphereRegrid NP=4 exfrun

#
# ESMF_FieldMeshRegridEx
#
run_ESMF_FieldMeshRegridEx:
#	cp -rf  ESMF_FieldMeshRegridExData $(ESMF_EXDIR)
	$(MAKE) EXNAME=FieldMeshRegrid NP=4 exfrun

#
# ESMF_FieldRepDimEx
#
run_ESMF_FieldRepDimEx:
	$(MAKE) EXNAME=FieldRepDim NP=4 exfrun

#
# ESMF_FieldRepDimEx
#
run_ESMF_FieldArbGridEx:
	$(MAKE) EXNAME=FieldArbGrid NP=4 exfrun

run_ESMF_FieldArbGridEx_uni:
	$(MAKE) EXNAME=FieldArbGrid NP=1 exfrun

#
# ESMF_FieldCommEx
#
run_ESMF_FieldCommEx:
	$(MAKE) EXNAME=FieldComm NP=4 exfrun


#
# ESMF_FieldRedistEx
#
run_ESMF_FieldRedistEx:
	$(MAKE) EXNAME=FieldRedist NP=4 exfrun


#
# ESMF_FieldSMMEx
#
run_ESMF_FieldSMMEx:
	$(MAKE) EXNAME=FieldSMM NP=4 exfrun


#
# ESMF_FieldHaloEx
#
run_ESMF_FieldHaloEx:
	$(MAKE) EXNAME=FieldHalo NP=4 exfrun


