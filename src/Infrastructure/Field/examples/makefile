# $Id: makefile,v 1.48 2009/05/07 22:46:23 oehmke Exp $ 

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
                 $(ESMF_EXDIR)/ESMF_FieldArbGridEx
#                 $(ESMF_EXDIR)/ESMF_FieldGetEx
#                 $(ESMF_EXDIR)/ESMF_FieldSetEx
#                 $(ESMF_EXDIR)/ESMF_FieldFromUserEx \
#                 $(ESMF_EXDIR)/ESMF_FieldGlobalEx \
#                 $(ESMF_EXDIR)/ESMF_FieldWriteEx

EXAMPLES_RUN = run_ESMF_FieldCreateEx \
               run_ESMF_FieldEx \
               run_ESMF_FieldRegridEx \
               run_ESMF_FieldRegridMaskEx \
               run_ESMF_FieldSphereRegridEx \
               run_ESMF_FieldMeshRegridEx \
               run_ESMF_FieldRepDimEx \
               run_ESMF_FieldArbGridEx

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
	cp -rf  ESMF_FieldMeshRegridExData $(ESMF_EXDIR)
	$(MAKE) EXNAME=FieldMeshRegrid NP=6 exfrun

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
# ESMF_FieldGetEx
#
run_ESMF_FieldGetEx:
	$(MAKE) EXNAME=FieldGet NP=4 exfrun
run_ESMF_FieldGetEx_uni:
	echo "PASS: ESMF_FieldGetEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldGetEx.stdout

#
# ESMF_FieldSetEx
#
run_ESMF_FieldSetEx:
	$(MAKE) EXNAME=FieldSet NP=4 exfrun
run_ESMF_FieldSetEx_uni:
	echo "PASS: ESMF_FieldSetEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldSetEx.stdout

#
# ESMF_FieldFromUserEx
#
ESMF_EXAMPLE_FieldFromUser_OBJS = UserCodeMod.o
ESMF_FieldFromUserEx.o	: UserCodeMod.o

run_ESMF_FieldFromUserEx:
	$(MAKE) EXNAME=FieldFromUser NP=6 exfrun

run_ESMF_FieldFromUserEx_uni:
	echo "PASS: ESMF_FieldFromUserEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldFromUserEx.stdout

#
# ESMF_FieldGlobalEx
#
run_ESMF_FieldGlobalEx:
	$(MAKE) EXNAME=FieldGlobal NP=6 exfrun

run_ESMF_FieldGlobalEx_uni:
	echo "PASS: ESMF_FieldGlobalEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldGlobalEx.stdout

#
# ESMF_FieldWriteEx
#
 run_ESMF_FieldWriteEx:
	$(MAKE) EXNAME=FieldWrite NP=4 exfrun

 run_ESMF_FieldWriteEx_uni:
	$(MAKE) EXNAME=FieldWrite NP=1 exfrun
